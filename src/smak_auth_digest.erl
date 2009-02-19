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

-module(smak_auth_digest).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([init/5]).

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

-record(conf, {
          realm :: string(),
          auth :: function(),
          nget :: function(),
          nset :: function()
         }).

%% @spec init(ewgi_app(), string(), function(), function(), function()) -> ewgi_app()
%% @doc Creates an application which checks for digest authentication
%% and executes the application if authentication is
%% successful. Otherwise, gives an error.
-spec init(ewgi_app(), string(), function(), function(), function()) -> ewgi_app().
init(Application, Realm, AuthFunc, NonceGet, NonceSet) ->
    Conf = #conf{realm=Realm, auth=AuthFunc, nget=NonceGet, nset=NonceSet},
    F = fun(Ctx0) ->
                case ewgi_api:remote_user(Ctx0) of
                    undefined ->
                        case authenticate(Ctx0, Conf) of
                            Result when is_list(Result) ->
                                Ctx1 = ewgi_api:auth_type("digest", Ctx0),
                                Ctx = ewgi_api:remote_user(Result, Ctx1),
                                ewgi_application:run(Application, Ctx);
                            App when ?IS_EWGI_APPLICATION(App) ->
                                ewgi_application:run(App, Ctx0)
                        end;
                    _ ->
                        ewgi_application:run(Application, Ctx0)
                end
        end,
    F.

-spec authenticate(#ewgi_context{}, #conf{}) -> string() | ewgi_app().
authenticate(Ctx0, Conf) ->
    Method = ewgi_api:request_method(Ctx0),
    Fullpath = ewgi_api:script_name(Ctx0) ++ ewgi_api:path_info(Ctx0),
    case ewgi_api:get_header_value("authorization", Ctx0) of
        Authorization when is_list(Authorization) ->
            [AuthMethod, Auth] = smak_string:split(Authorization, $\ , 1),
            case string:to_lower(AuthMethod) of
                "digest" ->
                    Tokens = smak_string:split(Auth, ", "),
                    AuthMap = lists:foldl(fun parse_kv/2, gb_trees:empty(), Tokens),
                    case (catch get_map_values(AuthMap, Fullpath, Conf)) of
                        D when is_record(D, dres) ->
                            Ha1 = (Conf#conf.auth)(Ctx0, Conf#conf.realm, D#dres.username),
                            compute(Ha1, Method, D, Conf);
                        _ ->
                            unauthorized(Conf)
                    end;
                _ ->
                    unauthorized(Conf)
            end;
        _ ->
            unauthorized(Conf)
    end.

-spec get_map_values(gb_tree(), string(), #conf{}) -> #dres{}.
get_map_values(AuthMap, Fullpath, #conf{realm=Realm}) ->
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

-spec compute(string(), string(), #dres{}, #conf{}) -> string() | ewgi_app().
compute([], _, _, Conf) ->
    unauthorized(Conf);
compute(Ha1, Method, D, Conf) when is_record(D, dres) ->
    Ha2 = smak_hex:to_hex(erlang:md5([Method, $:, D#dres.uri])),
    Chk = case D#dres.qop of
              [] ->
                  smak_hex:to_hex(erlang:md5([Ha1, $:, D#dres.nonce, $:, Ha2]));
              _ ->
                  smak_hex:to_hex(erlang:md5([Ha1, $:, D#dres.nonce, $:, D#dres.nc, $:, D#dres.cnonce, $:, D#dres.qop, $:, Ha2]))
          end,
    case D#dres.response of
        Chk ->
            Pnc = (Conf#conf.nget)(D#dres.nonce),
            if
                D#dres.nc =< Pnc ->
                    (Conf#conf.nset)(D#dres.nonce, undefined),
                    unauthorized(true);
                true ->
                    (Conf#conf.nset)(D#dres.nonce, D#dres.nc),
                    D#dres.username
            end;
        _ ->
            (Conf#conf.nset)(D#dres.nonce, undefined),
            unauthorized(Conf)
    end.

-spec assert_valid_authpath(string(), string()) -> 'ok' | {'error', 'not_in_string'}.
assert_valid_authpath(A, F) ->
    A1 = lists:takewhile(fun($\?) -> false; (_) -> true end, A),
    case string:str(F, A1) of
        0 ->
            {error, not_in_string};
        _ ->
            ok
    end.

-spec assert_valid_qop(list()) -> 'ok' | 'invalid'.
assert_valid_qop([]) ->
    ok;
assert_valid_qop("auth") ->
    ok;
assert_valid_qop(_) ->
    invalid.

-spec lookup_default(any(), gb_tree(), T) -> any() | T.
lookup_default(Key, Tree, Default) ->
    case gb_trees:lookup(Key, Tree) of
        none ->
            Default;
        {value, V} ->
            V
    end.

-spec parse_kv(string(), gb_tree()) -> gb_tree().
parse_kv(Str, Tree) ->
    [K, V] = smak_string:split(Str, $\=, 1),
    gb_trees:enter(smak_string:strip(K),
                   smak_string:strip(smak_string:strip(V), both, "\""),
                   Tree).

-spec unauthorized(#conf{}) -> ewgi_app().
unauthorized(Conf) ->
    unauthorized(false, Conf).

-spec unauthorized(bool(), #conf{}) -> ewgi_app().
unauthorized(Stale, Conf) ->
    {NowDt, NowMs} = smak_calendar:now_utc_ms(),
    Now = smak_calendar:now_to_unix_ts(NowDt, NowMs),
    Rand = smak_random:uniform(),
    Nonce = smak_hex:to_hex(erlang:md5(io_lib:format("~.6f:~.16f", [Now, Rand]))),
    Opaque = smak_hex:to_hex(erlang:md5(io_lib:format("~.6f:~.16f", [Now, Rand]))),
    (Conf#conf.nset)(Nonce, undefined),
    Head = get_digest_head(Nonce, Opaque, Stale, Conf),
    BR = ["Digest ", Head],
    H = [{"WWW-Authenticate", BR}],
    smak_http_status:unauthorized([], H, []).

-spec get_digest_head(string(), string(), bool(), #conf{}) -> string().
get_digest_head(Nonce, Opaque, Stale, Conf) ->
    Pairs = lists:map(fun hpair/1, get_digest_head_list({Nonce, Opaque}, Stale, Conf)),
    string:join(Pairs, ", ").

-spec get_digest_head_list({string(), string()}, bool(), #conf{}) -> [{string(), string()}].
get_digest_head_list(A, true, Conf) ->
    [{"stale", "true"}|get_digest_head_list(A, false, Conf)];
get_digest_head_list({Nonce, Opaque}, false, Conf) ->
    [{"realm", Conf#conf.realm}, {"qop", "auth"}, {"nonce", Nonce}, {"opaque", Opaque}].

-spec hpair({string(), string()}) -> string().
hpair({Key, Val}) ->
    [Key, $=, $\", Val, $\"].

