%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% TODO: Provide mechanism to include in supervision tree.
%% TODO: Add flexible formatting
%% 
%% @doc Smak file logging gen_event handler.  Primarily for use with
%% smak_log.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_log_file).
-author('Hunter Morris <hunter.morris@smarkets.com>').
-behaviour(gen_event).

-export([add/1, add/2, delete/0, delete/1]).
-export([init/1, handle_event/2, terminate/2, code_change/3, handle_call/2, handle_info/2]).

-include("smak.hrl").

-type slf_option() :: {'filename', string()} | {'level', log_level()}.

-record(slf_state, {
          fd    :: any(),
          level :: log_level()
         }).

-spec add([slf_option()]) -> 'ok'.
add(Options) ->    
    add(?EWGI_LOGGER_NAME, Options).

-spec add(atom(), [slf_option()]) -> 'ok'.
add(Name, Options) ->
    gen_event:add_handler(Name, ?MODULE, Options).

-spec delete() -> 'ok'.
delete() ->
    delete(?EWGI_LOGGER_NAME).
    
-spec delete(atom()) -> 'ok'.
delete(Name) ->
    gen_event:delete_handler(Name, ?MODULE, []).

-spec init([slf_option()]) -> {'ok', #slf_state{}}.
init(Options) ->
    Filename = proplists:get_value(filename, Options, "smak.log"),
    Level = proplists:get_value(level, Options, info),
    {ok, Fd} = file:open(Filename, [append]),
    {ok, #slf_state{fd=Fd, level=Level}}.

handle_event({log, {Pid, L, F, A}}, #slf_state{fd=Fd, level=Level}=S) ->
    case smak_log:level_compare(L, Level) of
        true ->
            Msg = lists:flatten(io_lib:format(F, A)),
            ok = io:format(Fd, "~p.~n", [{httpd_util:rfc1123_date(), L, pid_to_list(Pid), Msg}]),
            {ok, S};
        false ->
            {ok, S}
    end.

terminate(_, #slf_state{fd=Fd}) ->
    file:close(Fd).

handle_call(get_level, #slf_state{level=Level}=State) ->
    {ok, Level, State};
handle_call({set_level_integer, NewLevel}, State) ->
    {ok, ok, State#slf_state{level=NewLevel}}.

handle_info(_, State) ->
    {ok, State}.

code_change(_, State, _) ->
    {ok, State}.
