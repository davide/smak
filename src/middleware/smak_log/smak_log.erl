%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% TODO: Provide mechanism to include in supervision tree.
%% 
%% @doc Smak basic logging middleware.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_log).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([start_link/0, start_link/1]).
-export([init/1, level_compare/2]).

-include("smak.hrl").

%% @type sl_option() = {name, atom()}
-type sl_option() :: {'name', atom()}.

-type start_result() :: {'ok', pid()} | {error, {'already_started', pid()}} | {error, any()}.

-spec start_link() -> start_result().
start_link() ->
    start_link({local, ?EWGI_LOGGER_NAME}).

-spec start_link({'local', atom()} | {'global', atom()}) -> start_result().
start_link(Name) ->
    gen_event:start_link(Name).

%% @spec init(Options::[sl_option()]) -> ewgi_app()
%% @doc Initialises the logging middleware which provides a function
%% for logging from an application.
-spec init([sl_option()]) -> ewgi_app().
init(Options) when is_list(Options) ->
    Name = proplists:get_value(name, Options, ?EWGI_LOGGER_NAME),
    F = fun(Ctx0) ->
                ewgi_api:store_data(?EWGI_LOGGER_KEY, log_event(Name), Ctx0)
        end,
    F.
               
-spec log_event(atom()) -> fun((log_level(), string(), [any()]) -> 'ok').
log_event(Name) ->
    F = fun(L, F, A) ->
                gen_event:notify(Name, {log, {self(), L, F, A}})
        end,
    F.

-spec level_compare(log_level(), log_level()) -> boolean().
level_compare(error, error) ->
    true;
level_compare(error, info) ->
    true;
level_compare(error, debug) ->
    true;
level_compare(error, verbose) ->
    true;
level_compare(info, info) ->
    true;
level_compare(info, debug) ->
    true;
level_compare(info, verbose) ->
    true;
level_compare(debug, debug) ->
    true;
level_compare(debug, verbose) ->
    true;
level_compare(verbose, verbose) ->
    true;
level_compare(_, _) ->
    false.
