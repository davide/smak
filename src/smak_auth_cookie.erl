%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.
%%
%% @doc Smak cookie-based session/authentication middleware.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_auth_cookie).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([init/3]).

-include("smak.hrl").

-compile(export_all).

-define(DEFAULT_COOKIE_NAME, "smak_auth").
-define(DEFAULT_ENCODER, smak_sn_cookie).
-define(SET_USER_ENV_NAME, "smak.auth_cookie.set_user").
-define(LOGOUT_USER_ENV_NAME, "smak.auth_cookie.logout_user").
-define(DEFAULT_IP, "0.0.0.0").
-define(COOKIE_DELETE_TRAILER, "; Expires=Thu, 01 Jan 1970 23:00:00 GMT; Max-Age=0").

-record(cka, {
          application = undefined,
          key = undefined,
          cookie_name = ?DEFAULT_COOKIE_NAME,
          encoder = ?DEFAULT_ENCODER,
          include_ip = false,
          timeout = 15 * 60 * 1000, %% 15 minutes
          maxlength = 4096,
          secure = false
         }).

%% External API

%% @spec init(Application::ewgi_app(), Key::binary(), Options::proplist()) -> ewgi_app()
%% @doc Initializes authentication middleware and returns the appropriate application.
-spec(init/3 :: (ewgi_app(), binary(), proplist()) -> ewgi_app()).

init(Application, Key, Options) when is_binary(Key), size(Key) >= 16 ->
    case populate_record(Options, #cka{application=Application, key=Key}) of
        Cka when is_record(Cka, cka) ->
            F = fun(Env, StartResp) ->
                        handle_request(Env, StartResp, Cka)
                end,
            F;
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, invalid_parameters}
    end.

handle_request(Env0, StartResp, Cka) ->
    Env = decode_cookies(smak_ewgi:http_cookie(Env0), Env0, Cka),
    smak_ewgi:call_application(Cka#cka.application, add_funs(Env, Cka), StartResp).

decode_cookies(undefined, Env, _) ->
    Env;
decode_cookies(Cookie, Env, Cka) ->
    CookieValues = smak_cookie:parse_cookie(Cookie),
    decode_cookies1(proplists:get_value(Cka#cka.cookie_name, CookieValues), Env, Cka).

decode_cookies1(undefined, Env, _) ->
    Env;
decode_cookies1(Cookie0, Env, Cka) ->
    Cookie = cookie_safe_decode(Cookie0),
    case Cookie of
        <<>> ->
            Env;
        Cookie when is_binary(Cookie) ->
            case (Cka#cka.encoder):decode(Cka#cka.key, Cookie, Cka#cka.timeout) of
                {error, _Reason} ->
                    %% TODO: Warn?
                    Env;
                Content when is_binary(Content) ->
                    case (catch(binary_to_term(Content))) of
                        {'EXIT', _} ->
                            Env;
                        Values when is_list(Values) ->
                            case lists:foldl(scan_cookie_content(Cka), Env, Values) of
                                {error, _} ->
                                    %% TODO: Warn?
                                    Env;
                                ModifiedEnv ->
                                    ModifiedEnv
                            end;
                        _ ->
                            Env
                    end;
                _ ->
                    Env
            end
    end.

scan_cookie_content(#cka{include_ip=I}) ->
    F = fun({"REMOTE_USER", U}, Env) ->
                smak_ewgi:remote_user(Env, U);
           ({"REMOTE_USER_DATA", D}, Env) ->
                smak_ewgi:remote_user_data(Env, D);
           ({"REMOTE_ADDR", SavedAddr}, Env) when I =:= true ->
                Addr = smak_ewgi:remote_addr(Env),
                if SavedAddr =:= Addr ->
                        Env;
                   true ->
                        error_logger:error_msg("Saved address: ~p, New address: ~p", [SavedAddr, Addr]),
                        {error, invalid_ip_address}
                end;
           (_, Env) ->
                %% TODO: Warn?
                Env
        end,
    F.

add_funs(Env0, Cka) ->
    lists:foldl(fun({K, V}, Env) ->
                        smak_ewgi:env_set(Env, K, V)
                end,
                Env0,
                [{?SET_USER_ENV_NAME, set_user(Env0, Cka)},
                 {?LOGOUT_USER_ENV_NAME, logout_user(Env0, Cka)}]).

set_user(Env, #cka{include_ip=IncludeIp, cookie_name=CookieName, encoder=Encoder, key=Key, maxlength=M, secure=Sec}) ->
    F = fun(UserId, UserData) ->
                RemoteAddr = if IncludeIp -> smak_ewgi:remote_addr(Env);
                                true -> ?DEFAULT_IP
                             end,
                CookieData = [{"REMOTE_USER", UserId}, {"REMOTE_USER_DATA", UserData}, {"REMOTE_ADDR", RemoteAddr}],
                CookieVal = case Encoder:encode(Key, term_to_binary(CookieData), M) of
                                {error, _Reason} ->
                                    [];
                                B ->
                                    cookie_safe_encode(B)
                            end,
                {CurDomain, WildDomain} = get_domains(Env),
                [simple_cookie(CookieName, CookieVal, Sec),
                 simple_cookie(CookieName, CookieVal, Sec, CurDomain),
                 simple_cookie(CookieName, CookieVal, Sec, WildDomain)]
        end,
    F.

logout_user(Env, #cka{cookie_name=CookieName}) ->
    F = fun() ->
                {CurDomain, WildDomain} = get_domains(Env),
                [simple_cookie(CookieName, [], false),
                 simple_cookie(CookieName, [], false, CurDomain),
                 simple_cookie(CookieName, [], false, WildDomain)]
        end,
    F.

simple_cookie(Name, Val, Sec) when is_binary(Val) ->
    simple_cookie(Name, binary_to_list(Val), Sec);
simple_cookie(Name, Val, Sec) when is_list(Name), is_list(Val) ->
    S = if Sec -> "; Secure"; true -> [] end,
    Exp = case Val of [] -> ?COOKIE_DELETE_TRAILER; _ -> [] end,
    {"Set-Cookie", [Name, $=, Val, "; Path=/", S, Exp]}.

simple_cookie(Name, Val, Sec, Domain) when is_binary(Val) ->
    simple_cookie(Name, binary_to_list(Val), Sec, Domain);
simple_cookie(Name, Val, Sec, Domain) when is_binary(Domain) ->
    simple_cookie(Name, Val, Sec, binary_to_list(Domain));
simple_cookie(Name, Val, Sec, Domain) ->
    S = if Sec -> "; Secure"; true -> [] end,
    Exp = case Val of [] -> ?COOKIE_DELETE_TRAILER; _ -> [] end,
    {"Set-Cookie", io_lib:format("~s=~s; Path=/; Domain=~s~s~s", [Name, Val, Domain, S, Exp])}.

get_domains(Env) ->
    Cur = case smak_ewgi:http_host(Env) of
              undefined ->
                  smak_ewgi:server_name(Env);
              H ->
                  H
          end,
    Wild = [$.|Cur],
    {Cur, Wild}.

cookie_safe_encode(Bin) when is_binary(Bin) ->
    Enc = binary_to_list(base64:encode(Bin)),
    list_to_binary(cookie_safe_encode1(Enc, [])).

cookie_safe_encode1([], Acc) ->
    lists:reverse(Acc);
cookie_safe_encode1([$=|Rest], Acc) ->
    cookie_safe_encode1(Rest, [$~|Acc]);
cookie_safe_encode1([$/|Rest], Acc) ->
    cookie_safe_encode1(Rest, [$_|Acc]);
cookie_safe_encode1([C|Rest], Acc) ->
    cookie_safe_encode1(Rest, [C|Acc]).

cookie_safe_decode(Bin) when is_binary(Bin) ->
    cookie_safe_decode(binary_to_list(Bin));
cookie_safe_decode(L) when is_list(L) ->
    Dec = cookie_safe_decode1(L, []),
    base64:decode(Dec).

cookie_safe_decode1([], Acc) ->
    lists:reverse(Acc);
cookie_safe_decode1([$~|Rest], Acc) ->
    cookie_safe_decode1(Rest, [$=|Acc]);
cookie_safe_decode1([$_|Rest], Acc) ->
    cookie_safe_decode1(Rest, [$/|Acc]);
cookie_safe_decode1([C|Rest], Acc) ->
    cookie_safe_decode1(Rest, [C|Acc]).

populate_record(Options, Cka) ->
    lists:foldr(fun r_option/2, Cka, Options).

r_option({cookie_name, N}, Cka) when is_list(N), is_record(Cka, cka) ->
    Cka#cka{cookie_name=N};
r_option({encoder, E}, Cka) when is_record(Cka, cka) ->
    Cka#cka{encoder=E};
r_option({include_ip, true}, Cka) when is_record(Cka, cka) ->
    Cka#cka{include_ip=true};
r_option({timeout, T}, Cka) when is_integer(T), is_record(Cka, cka) ->
    Cka#cka{timeout=T};
r_option({maxlength, M}, Cka) when is_integer(M), M > 0, is_record(Cka, cka) ->
    Cka#cka{maxlength=M};
r_option({secure, true}, Cka) when is_record(Cka, cka) ->
    Cka#cka{secure=true};
r_option(O, _Cka) ->
    {error, {invalid_option, O}}.
