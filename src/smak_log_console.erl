%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% TODO: Provide mechanism to include in supervision tree.
%% TODO: Add flexible formatting
%% 
%% @doc Smak console logging gen_event handler.  Primarily for use with
%% smak_log.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_log_console).
-author('Hunter Morris <hunter.morris@smarkets.com>').
-behaviour(gen_event).

-export([add/2, delete/1]).
-export([init/1, handle_event/2, terminate/2, code_change/3, handle_call/2, handle_info/2]).

-include("smak.hrl").

-type slc_option() :: {'level', log_level()}.

-record(slc_state, {
          level :: log_level()
         }).

-spec add(atom(), [slc_option()]) -> ok.
add(Name, Options) ->
    gen_event:add_handler(Name, ?MODULE, Options).

-spec delete(atom()) -> 'ok'.
delete(Name) ->
    gen_event:delete_handler(Name, ?MODULE, []).

-spec init([slc_option()]) -> {'ok', #slc_state{}}.
init(Options) ->
    Level = proplists:get_value(level, Options, info),
    {ok, #slc_state{level=Level}}.

handle_event({log, {Pid, L, F, A}}, #slc_state{level=Level}=S) ->
    case smak_log:level_compare(L, Level) of
        true ->
            Msg = io_lib:format(F, A),
            ok = io:format("[~s] [~s] [~p] ~s~n", [httpd_util:rfc1123_date(), L, Pid, Msg]),
            {ok, S};
        false ->
            {ok, S}
    end.

terminate(_, _) ->
    ok.

handle_call(get_level, #slc_state{level=Level}=State) ->
    {ok, Level, State};
handle_call({set_level_integer, NewLevel}, State) ->
    {ok, ok, State#slc_state{level=NewLevel}}.

handle_info(_, State) ->
    {ok, State}.

code_change(_, State, _) ->
    {ok, State}.
