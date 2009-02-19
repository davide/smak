%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.
%%
%% @doc Smak basic authentication middleware. This middleware should be
%% avoided unless you need to support older HTTP 1.0 clients over SSL. Digest
%% authentication is probably more appropriate.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/

-module(smak_auth_basic).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([init/3]).

-include("smak.hrl").

%% @spec init(ewgi_app(), Realm::string(), Auth::function()) -> ewgi_app().
%% @doc Initializes basic authentication middleware.
-spec init(ewgi_app(), string(), function()) -> ewgi_app().
init(Application, Realm, AuthFunc) when ?IS_EWGI_APPLICATION(Application),
                                        is_list(Realm),
                                        is_function(AuthFunc, 3) ->
    F = fun(Ctx0) ->
                case ewgi_api:remote_user(Ctx0) of
                    undefined ->
                        case authenticate(Ctx0, Realm, AuthFunc) of
                            Result when is_list(Result) ->
                                Ctx1 = ewgi_api:auth_type("basic", Ctx0),
                                Ctx = ewgi_api:remote_user(Result, Ctx1),
                                %% Success; serve the app
                                ewgi_application:run(Application, Ctx);
                            App when ?IS_EWGI_APPLICATION(App) ->
                                ewgi_application:run(App, Ctx0);
                            Other -> % should be a ewgi_context
                                Other
                        end;
                    _ -> % Remote user already defined
                        ewgi_application:run(Application, Ctx0)
                end
        end,
    F.

%% Tests the environment for authorization and either returns the
%% authenticated username or an ewgi application which gives an
%% "Unauthorized" response.
-spec authenticate(#ewgi_context{}, string(), function()) -> ewgi_app() | string().
authenticate(Ctx0, Realm, AuthFunc) ->
    case ewgi_api:get_header_value("authorization", Ctx0) of
        Authorization when is_list(Authorization) ->
            [AuthMethod, Auth] = smak_string:split(Authorization, $\ , 1),
            case string:to_lower(AuthMethod) of
                "basic" ->
                    Auth1 = base64:decode_to_string(smak_string:strip(Auth)),
                    [Username, Password] = smak_string:split(Auth1, $:, 1),
                    case AuthFunc(Ctx0, Username, Password) of
                        true -> Username;
                        false -> unauthorized(Ctx0, Realm)
                    end;
                _ ->
                    unauthorized(Ctx0, Realm)
            end;
        _ ->
            unauthorized(Ctx0, Realm)
    end.

%% Prompts for basic authentication giving a 401 Unauthorized response.
-spec unauthorized(#ewgi_context{}, Realm::string()) -> ewgi_app().
unauthorized(_Ctx0, Realm) ->
    H = [{"WWW-Authenticate", io_lib:format("Basic realm=\"~s\"", [Realm])}],
    smak_http_status:unauthorized([], H, []).

