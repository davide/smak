%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.
%%
%% @doc Smak HTTP response utility methods.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/

-module(smak_response).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([replace_header/3]).

-include("smak.hrl").

%% @spec replace_header(proplist(), string(), string()) -> {string(), proplist()}
%% @doc Replace the first occurrence of the header specified with the
%% value. If the header does not exist, add a new one. Note that this method
%% should not be used for header names such as "set-cookie" which may appear
%% more than once in the headers. Returns the new header list and the
%% previous value (if exists).
-spec(replace_header/3 :: (proplist(), string(), string()) -> {string(), proplist()}).

replace_header(Headers, Name, Value) ->
    Name1 = string:to_lower(Name),
    replace_header1(Headers, Name1, Value, [], []).

replace_header1([], _, _, Prev, Acc) ->
    {Prev, lists:reverse(Acc)};
replace_header1([{PrevName, PrevValue}|T], Name, Value, [], Acc) ->
    case string:to_lower(PrevName) of
        Name ->
            replace_header1(T, Name, Value, PrevValue, [{Name, Value}|Acc]);
        _ ->
            replace_header1(T, Name, Value, [], [{PrevName, PrevValue}|Acc])
    end;
replace_header1([H|T], Name, Value, Prev, Acc) ->
    replace_header1(T, Name, Value, Prev, [H|Acc]).
