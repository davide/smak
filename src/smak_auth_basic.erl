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

-module(smak_auth_basic, [Application, Realm, AuthFunc]).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([init/1, handle_request/2]).

-include("smak.hrl").

%% External API

%% @spec init(any()) -> ok
%% @doc Initializes authentication middleware.
-spec(init/1 :: (any()) -> ok).
             
init(_) ->
    ok.

%% @spec handle_request(ewgi_env(), ewgi_start_response()) -> ewgi_response()
%% @doc Checks for basic authentication and executes the application if
%% authentication is successful. Otherwise, gives an error.
-spec(handle_request/2 :: (ewgi_env(), ewgi_start_response()) -> ewgi_response()).
              
handle_request(Env, StartResp) ->
    case smak_ewgi:remote_user(Env) of
        undefined ->
            case authenticate(Env) of
                Result when is_list(Result) ->
                    Env1 = smak_ewgi:auth_type(Env, "basic"),
                    Env2 = smak_ewgi:remote_user(Env1, Result),
                    %% Authentication was successful; serve the app
                    smak_ewgi:call_application(Application, Env2, StartResp);
                F when is_function(F) ->
                    F(Env, StartResp)
            end;
        _ -> % Remote user already defined
            smak_ewgi:call_application(Application, Env, StartResp)
    end.

%% Tests the environment for authorization and either returns the
%% authenticated username or an ewgi application which gives an
%% "Unauthorized" response.
-spec(authenticate/1 :: (ewgi_env()) -> string() | ewgi_app()).

authenticate(Env) ->
    case smak_ewgi:env_get(Env, "HTTP_AUTHORIZATION") of
        Authorization when is_list(Authorization) ->
            [AuthMethod, Auth] = smak_string:split(Authorization, $\ , 1),
            case string:to_lower(AuthMethod) of
                "basic" ->
                    Auth1 = base64:decode_to_string(smak_string:strip(Auth)),
                    [Username, Password] = smak_string:split(Auth1, $:, 1),
                    case AuthFunc(Env, Username, Password) of
                        true -> Username;
                        false -> unauthorized(Env)
                    end;
                _ ->
                    unauthorized(Env)
            end;
        _ ->
            unauthorized(Env)
    end.

%% Prompts for basic authentication giving a 401 Unauthorized response.
-spec(unauthorized/1 :: (ewgi_env()) -> ewgi_app()).

unauthorized(_Env) ->
    H = [{"WWW-Authenticate",
          io_lib:format("Basic realm=\"~s\"", [Realm])}],
    smak_http_status:unauthorized([], H, []).

