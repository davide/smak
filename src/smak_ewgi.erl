%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.

%% @doc Collection of methods for dealing with EWGI environments.

-module(smak_ewgi).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([call_application/3,
         remote_user/1,
         remote_user/2,
         remote_session/1,
         remote_session/2,
         auth_type/1,
         auth_type/2,
         request_method/1,
         request_method/2,
         script_name/1,
         script_name/2,
         path_info/1,
         path_info/2,
         env_get/2,
         env_set/3,
         env_append/3]).
         

-include("smak.hrl").

-define(ENV_REMOTE_USER, "REMOTE_USER").
-define(ENV_REMOTE_SESSION, "REMOTE_SESSION").
-define(ENV_AUTH_TYPE, "AUTH_TYPE").
-define(ENV_REQUEST_METHOD, "REQUEST_METHOD").
-define(ENV_SCRIPT_NAME, "SCRIPT_NAME").
-define(ENV_PATH_INFO, "PATH_INFO").

%% External API
%% @spec call_application(ewgi_app(), proplist(), function()) -> ewgi_response()
%% @doc Invoke an application
-spec(call_application/3 :: (ewgi_app(), proplist(), function()) -> ewgi_response()).

call_application(App, Env, StartResp) when is_function(App, 2) ->
    App(Env, StartResp);
call_application(App, Env, StartResp) -> %%when is_tuple(App); is_atom(App) ->
    App:handle_request(Env, StartResp).

%% @spec remote_user(proplist()) -> undefined | string()
%% @doc Get the REMOTE_USER value from the environment.
-spec(remote_user/1 :: (proplist()) -> undefined | string()).

remote_user(Env) when is_list(Env) ->
    env_get(Env, ?ENV_REMOTE_USER).

%% @spec remote_user(proplist(), string()) -> proplist()
%% @doc Sets the REMOTE_USER value in the environment.
-spec(remote_user/2 :: (proplist(), string()) -> undefined | any()).

remote_user(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_REMOTE_USER, Value).

%% @spec remote_session(proplist()) -> undefined | string()
%% @doc Get the REMOTE_SESSION value from the environment.
-spec(remote_session/1 :: (proplist()) -> undefined | string()).

remote_session(Env) when is_list(Env) ->
    env_get(Env, ?ENV_REMOTE_SESSION).

%% @spec remote_session(proplist(), string()) -> proplist()
%% @doc Sets the REMOTE_SESSION value in the environment.
-spec(remote_session/2 :: (proplist(), string()) -> undefined | any()).

remote_session(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_REMOTE_SESSION, Value).

%% @spec auth_type(proplist()) -> undefined | string()
%% @doc Get the AUTH_TYPE value from the environment.
-spec(auth_type/1 :: (proplist()) -> undefined | string()).

auth_type(Env) when is_list(Env) ->
    env_get(Env, ?ENV_AUTH_TYPE).

%% @spec auth_type(proplist(), string()) -> proplist()
%% @doc Sets the AUTH_TYPE value in the environment.
-spec(auth_type/2 :: (proplist(), string()) -> undefined | any()).

auth_type(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_AUTH_TYPE, Value).

%% @spec request_method(proplist()) -> undefined | string()
%% @doc Get the REQUEST_METHOD value from the environment.
-spec(request_method/1 :: (proplist()) -> undefined | string()).

request_method(Env) when is_list(Env) ->
    env_get(Env, ?ENV_REQUEST_METHOD).

%% @spec request_method(proplist(), string()) -> proplist()
%% @doc Sets the REQUEST_METHOD value in the environment.
-spec(request_method/2 :: (proplist(), string()) -> undefined | any()).

request_method(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_REQUEST_METHOD, Value).

%% @spec script_name(proplist()) -> undefined | string()
%% @doc Get the SCRIPT_NAME value from the environment.
-spec(script_name/1 :: (proplist()) -> undefined | string()).

script_name(Env) when is_list(Env) ->
    env_get(Env, ?ENV_SCRIPT_NAME).

%% @spec script_name(proplist(), string()) -> proplist()
%% @doc Sets the SCRIPT_NAME value in the environment.
-spec(script_name/2 :: (proplist(), string()) -> undefined | any()).

script_name(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_SCRIPT_NAME, Value).

%% @spec path_info(proplist()) -> undefined | string()
%% @doc Get the PATH_INFO value from the environment.
-spec(path_info/1 :: (proplist()) -> undefined | string()).

path_info(Env) when is_list(Env) ->
    env_get(Env, ?ENV_PATH_INFO).

%% @spec path_info(proplist(), string()) -> proplist()
%% @doc Sets the PATH_INFO value in the environment.
-spec(path_info/2 :: (proplist(), string()) -> undefined | any()).

path_info(Env, Value) when is_list(Env), is_list(Value) ->
    env_set(Env, ?ENV_PATH_INFO, Value).

%% @spec env_get(proplist(), any()) -> undefined | any()
%% @doc Get a value from the environment
-spec(env_get/2 :: (proplist(), any()) -> undefined | any()).

env_get(Env, Key) when is_list(Env) ->
    proplists:get_value(Key, Env).

%% @spec env_set(proplist(), any(), any()) -> proplist()
%% @doc Set a value in the environment
-spec(env_set/3 :: (proplist(), any(), any()) -> proplist()).

env_set(Env, Key, Value) when is_list(Env) ->
    [proplists:property(Key, Value)|proplists:delete(Key, Env)].

%% @spec env_append(proplist(), any(), any()) -> proplist()
%% @doc Append a value to a particular key in the environment. Values are
%% typically separated by commas.
-spec(env_append/3 :: (proplist(), any(), any()) -> proplist()).

env_append(Env, Key, Value) when is_list(Env) ->
    env_append1(proplists:get_value(Key, Env), Env, Key, Value).

env_append1(undefined, Env, Key, Value) ->
    [proplists:property(Key, Value)|Env];
env_append1(Str, Env, Key, Value) when is_list(Str) ->
    Str1 = string:join([Str, Value], ", "),
    env_set(Env, Key, Str1);
env_append1(Val, Env, Key, Value) ->
    Val1 = [Val, Value],
    env_set(Env, Key, Val1).
    

