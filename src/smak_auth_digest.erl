%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.
%%
%% @doc Smak digest authentication middleware. 
%% 
%% NonceGet is a fun/1 that takes a nonce and returns its counter (or 0 if it
%% isn't present). NonceSet is a fun/2 that takes a nonce and value (or
%% undefined). Because the stored nonce values are not stored by this module,
%% the NonceGet and NonceSet methods should most likely be implemented by a
%% separate process. The process dictionary is probably not an appropriate
%% place because the process in which this module runs is most likely owned
%% by the server implementation.
%%
%% The stateless recommendation from
%% RFC 2617 is: "Digesting the client IP and time-stamp in the nonce permits
%% an implementation which does not maintain state between transactions."
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/

-module(smak_auth_digest, [Application, Realm, AuthFunc, NonceGet, NonceSet]).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([handle_request/2]).

-include("smak.hrl").

-record(dres, {
          username :: string(),
          uri :: string(),
          nonce :: string(),
          response :: string(),
          qop :: string(),
          cnonce :: string(),
          nc :: string()
          }).

%% External API

%% @spec handle_request(ewgi_env(), ewgi_start_response()) -> ewgi_response()
%% @doc Checks for digest authentication and executes the application if
%% authentication is successful. Otherwise, gives an error.
-spec(handle_request/2 :: (ewgi_env(), ewgi_start_response()) -> ewgi_response()).
              
handle_request(Env, StartResp) ->
    case smak_ewgi:remote_user(Env) of
        undefined ->
            case authenticate(Env) of
                Result when is_list(Result) ->
                    Env1 = smak_ewgi:auth_type(Env, "digest"),
                    Env2 = smak_ewgi:remote_user(Env1, Result),
                    smak_ewgi:call_application(Application, Env2, StartResp);
                F when is_function(F, 2) ->
                    F(Env, StartResp)
            end;
        _ ->
            smak_ewgi:call_application(Application, Env, StartResp)
    end.

-spec(authenticate/1 :: (ewgi_env()) -> string() | ewgi_app()).
             
authenticate(Env) ->
    Method = smak_ewgi:request_method(Env),
    Fullpath = smak_ewgi:script_name(Env) ++ smak_ewgi:path_info(Env),
    case smak_ewgi:env_get(Env, "HTTP_AUTHORIZATION") of
        Authorization when is_list(Authorization) ->
            [AuthMethod, Auth] = smak_string:split(Authorization, $\ , 1),
            case string:to_lower(AuthMethod) of
                "digest" ->
                    Tokens = smak_string:split(Auth, ", "),
                    AuthMap = lists:foldl(fun parse_kv/2, gb_trees:empty(), Tokens),
                    case (catch get_map_values(AuthMap, Fullpath)) of
                        D when is_record(D, dres) ->
                            Ha1 = AuthFunc(Env, Realm, D#dres.username),
                            compute(Ha1, Method, D);
                        _ ->
                            unauthorized()
                    end;
                _ ->
                    unauthorized()
            end;
        _ ->
            unauthorized()
    end.


-spec(get_map_values/2 :: (gb_tree(), string()) -> #dres{}).
             
get_map_values(AuthMap, Fullpath) ->
    Username = gb_trees:get("username", AuthMap),
    Uri = gb_trees:get("uri", AuthMap),
    Nonce = gb_trees:get("nonce", AuthMap),
    Response = gb_trees:get("response", AuthMap),
    Qop = lookup_default("qop", AuthMap, []),
    Cnonce = lookup_default("cnonce", AuthMap, []),
    Nc = lookup_default("nc", AuthMap, "00000000"),
    ok = assert_valid_qop(Qop),
    ok = case Nonce of [] -> {error, invalid_nonce}; _ -> ok end,
    ok = case Nc of [] -> {error, invalid_nc}; _ -> ok end,
    Realm = gb_trees:get("realm", AuthMap),
    ok = assert_valid_authpath(Uri, Fullpath),
    #dres{username=Username,
          uri=Uri,
          nonce=Nonce,
          response=Response,
          qop=Qop,
          cnonce=Cnonce,
          nc=Nc}.

-spec(compute/3 :: (string(), string(), #dres{}) -> string() | ewgi_app()).
             
compute([], _, _) ->
    unauthorized();
compute(Ha1, Method, D) when is_record(D, dres) ->
    Ha2 = smak_hex:to_hex(erlang:md5(io_lib:format("~s:~s", [Method, D#dres.uri]))),
    Chk = case D#dres.qop of
              [] ->
                  smak_hex:to_hex(erlang:md5(io_lib:format("~s:~s:~s", [Ha1, D#dres.nonce, Ha2])));
              _ ->
                  smak_hex:to_hex(erlang:md5(io_lib:format("~s:~s:~s:~s:~s:~s", [Ha1, D#dres.nonce, D#dres.nc, D#dres.cnonce, D#dres.qop, Ha2])))
          end,
    case D#dres.response of
        Chk ->
            Pnc = NonceGet(D#dres.nonce),
            if
                D#dres.nc =< Pnc ->
                    NonceSet(D#dres.nonce, undefined),
                    unauthorized(true);
                true ->
                    NonceSet(D#dres.nonce, D#dres.nc),
                    D#dres.username
            end;
        _ ->
            NonceSet(D#dres.nonce, undefined),
            unauthorized()
    end.

-spec(assert_valid_authpath/2 :: (string(), string()) -> 'ok' | {'error', 'not_in_string'}).

assert_valid_authpath(A, F) ->
    A1 = lists:takewhile(fun($\?) -> false; (_) -> true end, A),
    case string:str(F, A1) of
        0 ->
            {error, not_in_string};
        _ ->
            ok
    end.

-spec(assert_valid_qop/1 :: (list()) -> 'ok' | 'invalid').

assert_valid_qop([]) ->
    ok;
assert_valid_qop("auth") ->
    ok;
assert_valid_qop(_) ->
    invalid.

-spec(lookup_default/3 :: (any(), gb_tree(), T) -> any() | T).

lookup_default(Key, Tree, Default) ->
    case gb_trees:lookup(Key, Tree) of
        none ->
            Default;
        {value, V} ->
            V
    end.

-spec(parse_kv/2 :: (string(), gb_tree()) -> gb_tree()).

parse_kv(Str, Tree) ->
    [K, V] = smak_string:split(Str, $\=, 1),
    gb_trees:enter(smak_string:strip(K),
                   smak_string:strip(smak_string:strip(V), both, "\""),
                   Tree).

-spec(unauthorized/0 :: () -> ewgi_app()).

unauthorized() ->
    unauthorized(false).

-spec(unauthorized/1 :: (bool()) -> ewgi_app()).

unauthorized(Stale) ->
    {NowDt, NowMs} = smak_calendar:now_utc_ms(),
    Now = smak_calendar:now_to_unix_ts(NowDt, NowMs),
    Rand = smak_random:uniform(),
    Nonce = smak_hex:to_hex(erlang:md5(io_lib:format("~.6f:~.16f", [Now, Rand]))),
    Opaque = smak_hex:to_hex(erlang:md5(io_lib:format("~.6f:~.16f", [Now, Rand]))),
    NonceSet(Nonce, undefined),
    Head = get_digest_head(Nonce, Opaque, Stale),
    BR = io_lib:format("Digest ~s", [Head]),
    H = [{"WWW-Authenticate", BR}],
    smak_http_status:unauthorized([], H, []).

-spec(get_digest_head/3 :: (string(), string(), bool()) -> string()).

get_digest_head(Nonce, Opaque, Stale) ->
    Pairs = lists:map(fun hpair/1, get_digest_head_list({Nonce, Opaque}, Stale)),
    string:join(Pairs, ", ").

-spec(get_digest_head_list/2 :: ({string(), string()}, bool()) -> [{string(), string()}]).
             
get_digest_head_list(A, true) ->
    [{"stale", "true"}|get_digest_head_list(A, false)];
get_digest_head_list({Nonce, Opaque}, false) ->
    [{"realm", Realm}, {"qop", "auth"}, {"nonce", Nonce}, {"opaque", Opaque}].

-spec(hpair/1 :: ({string(), string()}) -> string()).

hpair({Key, Val}) ->
    io_lib:format("~s=\"~s\"", [Key, Val]).
