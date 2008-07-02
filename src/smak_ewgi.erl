%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.

%% @doc Collection of methods for dealing with EWGI environments.

-module(smak_ewgi).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([call_application/3,
         url_scheme/1,
         url_scheme/2,
         remote_user/1,
         remote_user/2,
         remote_user_data/1,
         remote_user_data/2,
         remote_session/1,
         remote_session/2,
         remote_addr/1,
         remote_addr/2,
         auth_type/1,
         auth_type/2,
         request_method/1,
         request_method/2,
         script_name/1,
         script_name/2,
         server_name/1,
         server_name/2,
         path_info/1,
         path_info/2,
         content_type/1,
         content_type/2,
         http_accept/1,
         http_accept/2,
         http_cookie/1,
         http_cookie/2,
         http_host/1,
         http_host/2,
         env_get/2,
         env_set/3,
         env_append/3]).
         

-include("smak.hrl").

-define(ENV_URL_SCHEME, "ewgi.url_scheme").
-define(ENV_REMOTE_USER, "REMOTE_USER").
-define(ENV_REMOTE_USER_DATA, "REMOTE_USER_DATA").
-define(ENV_REMOTE_SESSION, "REMOTE_SESSION").
-define(ENV_REMOTE_ADDR, "REMOTE_ADDR").
-define(ENV_AUTH_TYPE, "AUTH_TYPE").
-define(ENV_REQUEST_METHOD, "REQUEST_METHOD").
-define(ENV_SCRIPT_NAME, "SCRIPT_NAME").
-define(ENV_SERVER_NAME, "SERVER_NAME").
-define(ENV_PATH_INFO, "PATH_INFO").
-define(ENV_HTTP_ACCEPT, "HTTP_ACCEPT").
-define(ENV_HTTP_COOKIE, "HTTP_COOKIE").
-define(ENV_HTTP_HOST, "HTTP_HOST").
-define(ENV_CONTENT_TYPE, "CONTENT_TYPE").

%% External API
%% @spec call_application(ewgi_app(), ewgi_env(), ewgi_start_response()) -> ewgi_response()
%% @doc Invoke an application
-spec(call_application/3 :: (ewgi_app(), ewgi_env(), ewgi_start_response()) -> ewgi_response()).

call_application(App, Env, StartResp) when is_function(App, 2) ->
    App(Env, StartResp);
call_application(App, Env, StartResp) -> %%when is_tuple(App); is_atom(App) ->
    App:handle_request(Env, StartResp).

%% @spec url_scheme(ewgi_env()) -> undefined | string()
%% @doc Get the URL_SCHEME value from the environment.
-spec(url_scheme/1 :: (ewgi_env()) -> 'undefined' | string()).

url_scheme(Env) when is_list(Env) ->
    env_get(Env, ?ENV_URL_SCHEME).

%% @spec url_scheme(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the URL_SCHEME value in the environment.
-spec(url_scheme/2 :: (ewgi_env(), string()) -> ewgi_env()).

url_scheme(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_URL_SCHEME, Value).

%% @spec remote_user(ewgi_env()) -> undefined | string()
%% @doc Get the REMOTE_USER value from the environment.
-spec(remote_user/1 :: (ewgi_env()) -> 'undefined' | string()).

remote_user(Env) when is_list(Env) ->
    env_get(Env, ?ENV_REMOTE_USER).

%% @spec remote_user(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the REMOTE_USER value in the environment.
-spec(remote_user/2 :: (ewgi_env(), string()) -> ewgi_env()).

remote_user(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_REMOTE_USER, Value).

%% @spec remote_user_data(ewgi_env()) -> undefined | string()
%% @doc Get the REMOTE_USER_DATA value from the environment.
-spec(remote_user_data/1 :: (ewgi_env()) -> 'undefined' | string()).

remote_user_data(Env) when is_list(Env) ->
    env_get(Env, ?ENV_REMOTE_USER_DATA).

%% @spec remote_user_data(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the REMOTE_USER_DATA value in the environment.
-spec(remote_user_data/2 :: (ewgi_env(), string()) -> ewgi_env()).

remote_user_data(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_REMOTE_USER_DATA, Value).

%% @spec remote_session(ewgi_env()) -> undefined | string()
%% @doc Get the REMOTE_SESSION value from the environment.
-spec(remote_session/1 :: (ewgi_env()) -> 'undefined' | string()).

remote_session(Env) when is_list(Env) ->
    env_get(Env, ?ENV_REMOTE_SESSION).

%% @spec remote_session(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the REMOTE_SESSION value in the environment.
-spec(remote_session/2 :: (ewgi_env(), string()) -> ewgi_env()).

remote_session(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_REMOTE_SESSION, Value).

%% @spec remote_addr(ewgi_env()) -> undefined | string()
%% @doc Get the REMOTE_ADDR value from the environment.
-spec(remote_addr/1 :: (ewgi_env()) -> 'undefined' | string()).

remote_addr(Env) when is_list(Env) ->
    env_get(Env, ?ENV_REMOTE_ADDR).

%% @spec remote_addr(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the REMOTE_ADDR value in the environment.
-spec(remote_addr/2 :: (ewgi_env(), string()) -> ewgi_env()).

remote_addr(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_REMOTE_ADDR, Value).

%% @spec auth_type(ewgi_env()) -> undefined | string()
%% @doc Get the AUTH_TYPE value from the environment.
-spec(auth_type/1 :: (ewgi_env()) -> 'undefined' | string()).

auth_type(Env) when is_list(Env) ->
    env_get(Env, ?ENV_AUTH_TYPE).

%% @spec auth_type(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the AUTH_TYPE value in the environment.
-spec(auth_type/2 :: (ewgi_env(), string()) -> ewgi_env()).

auth_type(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_AUTH_TYPE, Value).

%% @spec request_method(ewgi_env()) -> undefined | string()
%% @doc Get the REQUEST_METHOD value from the environment.
-spec(request_method/1 :: (ewgi_env()) -> 'undefined' | string()).

request_method(Env) when is_list(Env) ->
    env_get(Env, ?ENV_REQUEST_METHOD).

%% @spec request_method(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the REQUEST_METHOD value in the environment.
-spec(request_method/2 :: (ewgi_env(), string()) -> ewgi_env()).

request_method(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_REQUEST_METHOD, Value).

%% @spec script_name(ewgi_env()) -> undefined | string()
%% @doc Get the SCRIPT_NAME value from the environment.
-spec(script_name/1 :: (ewgi_env()) -> 'undefined' | string()).

script_name(Env) when is_list(Env) ->
    env_get(Env, ?ENV_SCRIPT_NAME).

%% @spec script_name(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the SCRIPT_NAME value in the environment.
-spec(script_name/2 :: (ewgi_env(), string()) -> ewgi_env()).

script_name(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_SCRIPT_NAME, Value).

%% @spec server_name(ewgi_env()) -> undefined | string()
%% @doc Get the SERVER_NAME value from the environment.
-spec(server_name/1 :: (ewgi_env()) -> 'undefined' | string()).

server_name(Env) when is_list(Env) ->
    env_get(Env, ?ENV_SERVER_NAME).

%% @spec server_name(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the SERVER_NAME value in the environment.
-spec(server_name/2 :: (ewgi_env(), string()) -> ewgi_env()).

server_name(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_SERVER_NAME, Value).

%% @spec path_info(ewgi_env()) -> undefined | string()
%% @doc Get the PATH_INFO value from the environment.
-spec(path_info/1 :: (ewgi_env()) -> 'undefined' | string()).

path_info(Env) when is_list(Env) ->
    env_get(Env, ?ENV_PATH_INFO).

%% @spec path_info(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the PATH_INFO value in the environment.
-spec(path_info/2 :: (ewgi_env(), string()) -> ewgi_env()).

path_info(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_PATH_INFO, Value).

%% @spec http_accept(ewgi_env()) -> undefined | string()
%% @doc Get the HTTP Accept header value from the environment.
-spec(http_accept/1 :: (ewgi_env()) -> 'undefined' | string()).

http_accept(Env) when is_list(Env) ->
    env_get(Env, ?ENV_HTTP_ACCEPT).

%% @spec http_accept(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the HTTP Accept value in the environment.
-spec(http_accept/2 :: (ewgi_env(), string()) -> ewgi_env()).

http_accept(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_HTTP_ACCEPT, Value).

%% @spec http_cookie(ewgi_env()) -> undefined | string()
%% @doc Get the HTTP Cookie header value from the environment.
-spec(http_cookie/1 :: (ewgi_env()) -> 'undefined' | string()).

http_cookie(Env) when is_list(Env) ->
    env_get(Env, ?ENV_HTTP_COOKIE).

%% @spec http_cookie(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the HTTP Cookie value in the environment.
-spec(http_cookie/2 :: (ewgi_env(), string()) -> ewgi_env()).

http_cookie(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_HTTP_COOKIE, Value).

%% @spec http_host(ewgi_env()) -> undefined | string()
%% @doc Get the HTTP Host header value from the environment.
-spec(http_host/1 :: (ewgi_env()) -> 'undefined' | string()).

http_host(Env) when is_list(Env) ->
    env_get(Env, ?ENV_HTTP_HOST).

%% @spec http_host(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the HTTP Host value in the environment.
-spec(http_host/2 :: (ewgi_env(), string()) -> ewgi_env()).

http_host(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_HTTP_HOST, Value).

%% @spec content_type(ewgi_env()) -> undefined | string()
%% @doc Get the Content-Type value from the environment.
-spec(content_type/1 :: (ewgi_env()) -> 'undefined' | string()).

content_type(Env) when is_list(Env) ->
    env_get(Env, ?ENV_CONTENT_TYPE).

%% @spec content_type(ewgi_env(), string()) -> ewgi_env()
%% @doc Sets the Content-Type value in the environment.
-spec(content_type/2 :: (ewgi_env(), string()) -> ewgi_env()).

content_type(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_CONTENT_TYPE, Value).

%% @spec env_get(ewgi_env(), any()) -> undefined | any()
%% @doc Get a value from the environment
-spec(env_get/2 :: (ewgi_env(), any()) -> 'undefined' | any()).

env_get(Env, Key) when is_list(Env) ->
    proplists:get_value(Key, Env).

%% @spec env_set(ewgi_env(), any(), any()) -> ewgi_env()
%% @doc Set a value in the environment
-spec(env_set/3 :: (ewgi_env(), any(), any()) -> ewgi_env()).

env_set(Env, Key, Value) when is_list(Env) ->
    [proplists:property(Key, Value)|proplists:delete(Key, Env)].

%% @spec env_append(ewgi_env(), any(), any()) -> ewgi_env()
%% @doc Append a value to a particular key in the environment. Values are
%% typically separated by commas.
-spec(env_append/3 :: (ewgi_env(), any(), any()) -> ewgi_env()).

env_append(Env, Key, Value) when is_list(Env) ->
    env_append1(proplists:get_value(Key, Env), Env, Key, Value).

-spec(env_append1/4 :: ('undefined' | any(), ewgi_env(), any(), any()) -> ewgi_env()).

env_append1(undefined, Env, Key, Value) ->
    [proplists:property(Key, Value)|Env];
env_append1(Str, Env, Key, Value) when is_list(Str) ->
    Str1 = string:join([Str, Value], ", "),
    env_set(Env, Key, Str1);
env_append1(Val, Env, Key, Value) ->
    Val1 = [Val, Value],
    env_set(Env, Key, Val1).
