%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak request timing.  Collects timing statistics about an
%% application.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_timer).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-include_lib("eunit/include/eunit.hrl").

-export([init/2]).

-include("smak.hrl").

%% @type timer_opt_key() = 'level'
-type timer_opt_key() :: 'level' | 'name'.
%% @type timer_opt() = {timer_opt_key(), any()}
-type timer_opt() :: {timer_opt_key(), any()}.

%% @spec init(App::ewgi_app(), Options::[timer_opt()]) -> ewgi_app()
%% @doc Initialises a timer middleware which measures the time it
%% takes for a request to be handled by App.
-spec init(ewgi_app(), [timer_opt()]) -> ewgi_app().
init(F, L) when is_list(L) ->
    Level = proplists:get_value(level, L, debug),
    Name = proplists:get_value(name, L, "Timer"),
    fun(Ctx) ->
            Before = erlang:now(),
            Val = F(Ctx), % FIXME: should I catch here?
            After = erlang:now(),
            Diff = timer:now_diff(After, Before),
            ?CTX_LOG(Val, Level, "[~s] Request took ~B microseconds.",
                     [Name, Diff]),
            Val
    end.
