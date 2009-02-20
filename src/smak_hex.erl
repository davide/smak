%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.
%%
%% @doc Tools for manipulating hexadecimal string representations of binary
%% data.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/
%%
%% This code is based on the mochihex module originally by Bob Ippolito of
%% Mochi Media, Inc.

-module(smak_hex).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([test/0, to_hex/1, to_bin/1, to_int/1, unhexdigit/1, hexdigit/1]).

-include("smak.hrl").

-import(erlang, [list_to_integer/2]).

%% @spec to_hex(integer() | iolist() | binary()) -> string()
%% @doc Convert an iolist to a hexadecimal string.
-spec to_hex(integer() | iolist() | binary()) -> string().
to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(L) when is_list(L); is_binary(L) ->
    to_hex(iolist_to_binary(L), []).

%% Helper methods
-spec to_hex(binary(), list()) -> string().
to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

-spec to_hex_int(integer(), list()) -> string().
to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

%% @spec to_bin(string()) -> binary()
%% @doc Convert a hexadecimal string to a binary.
-spec to_bin(string()) -> binary().
to_bin(L) ->
    to_bin(L, []).

%% Helper method
-spec to_bin(list(), list()) -> binary().
to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(unhexdigit(C1) bsl 4) bor unhexdigit(C2) | Acc]).

%% @spec to_int(string()) -> integer()
%% @doc Convert a hexadecimal string to an integer.
-spec to_int(string()) -> integer().
to_int(L) ->
    list_to_integer(L, 16).

%% @spec unhexdigit(char()) -> integer()
%% @doc Convert a hex digit to its integer value.
-spec unhexdigit(char()) -> integer().
unhexdigit(C) when C >= $0, C =< $9 ->
    C - $0;
unhexdigit(C) when C >= $a, C =< $f ->
    C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F ->
    C - $A + 10.

%% @spec hexdigit(integer()) -> char()
%% @doc Convert an integer less than 16 to a hex digit.
-spec hexdigit(integer()) -> char().
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

%% @spec test() -> ok
%% @doc Test this module.
-spec test() -> 'ok'.
test() ->
    "ff000ff1" = to_hex([255, 0, 15, 241]),
    <<255, 0, 15, 241>> = to_bin("ff000ff1"),
    16#ff000ff1 = to_int("ff000ff1"),
    "ff000ff1" = to_hex(16#ff000ff1),
    ok.
