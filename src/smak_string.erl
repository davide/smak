%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak miscellaneous string functions
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/

-module(smak_string).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([split/1, split/2, split/3, is_whitespace/1, strip/1, strip/2, strip/3]).
-export([partition/2]).

%% From http://unicode.org/Public/UNIDATA/PropList.txt
%% 0009..000D ; White_Space # Cc  [5] <control-0009>..<control-000D>
%% 0020       ; White_Space # Zs      SPACE
%% 0085       ; White_Space # Cc      <control-0085>
%% 00A0       ; White_Space # Zs      NO-BREAK SPACE
%% 1680       ; White_Space # Zs      OGHAM SPACE MARK
%% 180E       ; White_Space # Zs      MONGOLIAN VOWEL SEPARATOR
%% 2000..200A ; White_Space # Zs [11] EN QUAD..HAIR SPACE
%% 2028       ; White_Space # Zl      LINE SEPARATOR
%% 2029       ; White_Space # Zp      PARAGRAPH SEPARATOR
%% 202F       ; White_Space # Zs      NARROW NO-BREAK SPACE
%% 205F       ; White_Space # Zs      MEDIUM MATHEMATICAL SPACE
%% 3000       ; White_Space # Zs      IDEOGRAPHIC SPACE
-define(IS_WHITESPACE_GUARD(Ch), Ch >= 16#0009, Ch =< 16#000D;
        Ch =:= 16#0020; Ch =:= 16#00A0; Ch =:= 16#1680;
        Ch =:= 16#180E;
        Ch >= 16#2000, Ch =< 16#200A;
        Ch =:= 16#2028; Ch =:= 16#2029; Ch =:= 16#202F;
        Ch =:= 16#205F; Ch =:= 16#3000).

%% @type direction() = left | right | both
-type(direction() :: 'left' | 'right' | 'both').

%% @spec split(String::string()) -> [string()]
%% @doc Return a list of strings of the strings in String separated by any
%% whitespace characters.
-spec(split/1 :: (string()) -> [[char(),...]]).

split([]) ->
    [];
split(Str) when is_list(Str) ->
    split_whitespace(Str, -1, []).

%% @spec split(String::string(), Separators::char() | string() | whitespace) ->
%%       [string()]
%% @doc Return a list of strings of the strings in String, using Separators
%% as the delimiter string.
%% @see split/3
-spec(split/2 :: (string(), string() | char() | 'whitespace') -> [[char(),...]]).

split(Str, Seps) ->
    split(Str, Seps, -1).

%% @spec split(String::string(), Separators::char() | string() | whitespace,
%%       MaxSplit::integer()) -> [string()]
%% @doc Return a list of strings of the strings in String, using Separators
%% as the delimiter string. If MaxSplit is > 0, only MaxSplit splits will
%% occur (giving MaxSplit+1 resulting strings).
-spec(split/3 :: (string(), string() | char() | 'whitespace', integer()) -> [[char(),...]]).

split([], [], _N) ->
    [];
split([], _Seps, _N) ->
    [[]];
split(Str, _Seps, 0) ->
    Str;
split(Str, whitespace, N) when is_integer(N) ->
    split_whitespace(Str, N, []);
split(Str, Seps, N) when is_list(Seps), is_integer(N) ->
    split_substring(Str, Seps, N, [], []);
split(Str, Sep, N) when is_integer(Sep), is_integer(N) ->
    split_char(Str, Sep, N, [], []).

split_char(S, _Sep, 0, Toks, _Cs) ->
    lists:reverse([S|Toks]);
split_char([Sep|S], Sep, N, Toks, Cs) ->
    split_char(S, Sep, N-1, [lists:reverse(Cs)|Toks], []);
split_char([C|S], Sep, N, Toks, Cs) ->
    split_char(S, Sep, N, Toks, [C|Cs]);
split_char([], _Sep, _N, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

split_substring(S, _Seps, 0, Toks, _Cs) ->
    %% Note: _Cs should always be empty here
    lists:reverse([S|Toks]);
split_substring([C|S], [C|SRest]=Seps, N, Toks, Cs) ->
    case prefix(SRest, S) of
        true ->
            Str = string:substr(S, length(Seps)),
            split_substring(Str, Seps, N - 1, [lists:reverse(Cs)|Toks], []);
        false ->
            split_substring(S, Seps, N, Toks, [C|Cs])
    end;
split_substring([C|S], Seps, N, Toks, Cs) ->
    split_substring(S, Seps, N, Toks, [C|Cs]);
split_substring([], _Seps, _N, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

-spec(prefix/2 :: (string(), string()) -> bool()).
             
prefix([C|Pre], [C|String]) ->
    prefix(Pre, String);
prefix([], String) when is_list(String) ->
    true;
prefix(Pre, String) when is_list(Pre), is_list(String) ->
    false.

split_whitespace(Str, 0, Toks) ->
    lists:reverse([Str|Toks]);
split_whitespace([S|Rest], N, Toks) when ?IS_WHITESPACE_GUARD(S) ->
    split_whitespace(Rest, N, Toks);
split_whitespace([C|Rest], N, Toks) ->
    split_whitespace_acc(Rest, N, Toks, [C]);
split_whitespace([], _N, Toks) ->
    lists:reverse(Toks).

split_whitespace_acc([S|Rest], N, Toks, Cs) when ?IS_WHITESPACE_GUARD(S) ->
    split_whitespace(Rest, N-1, [lists:reverse(Cs)|Toks]);
split_whitespace_acc([C|Rest], N, Toks, Cs) ->
    split_whitespace_acc(Rest, N, Toks, [C|Cs]);
split_whitespace_acc([], _N, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

%% @spec strip(string()) -> string()
%% @doc Strips both left and right sides of string of any whitespace.
-spec(strip/1 :: (string()) -> string()).

strip(String) -> strip(String, both).

%% @spec strip(string(), Dir::direction()) -> string()
%% @doc Strips Dir sides of string of any whitespace.
-spec(strip/2 :: (string(), direction()) -> string()).

strip(String, left) ->
    strip_left(String);
strip(String, right) ->
    strip_right(String);
strip(String, both) ->
    strip_right(strip_left(String)).

%% @spec strip(string(), Dir::direction(), [char()]) -> string()
%% @doc Strips Dir of string of any characters specified.
-spec(strip/3 :: (string(), direction(), [char()]) -> string()).

strip(String, right, Chars) ->
    strip_right(String, Chars);
strip(String, left, Chars) ->
    strip_left(String, Chars);
strip(String, both, Chars) ->
    strip_right(strip_left(String, Chars), Chars).

strip_left([Sc|S]) when ?IS_WHITESPACE_GUARD(Sc) ->
    strip_left(S);
strip_left([_|_]=S) ->
    S;
strip_left([]) ->
    [].

strip_right([Sc|S]) when ?IS_WHITESPACE_GUARD(Sc) ->
    case strip_right(S) of
	[] -> [];
	T  -> [Sc|T]
    end;
strip_right([C|S]) ->
    [C|strip_right(S)];
strip_right([]) ->
    [].

strip_left([Ch|S]=Str, StripChars) ->
    case lists:member(Ch, StripChars) of
        true -> strip_left(S, StripChars);
        false -> Str
    end;
strip_left([], _StripChars) ->
    [].

strip_right([Ch|S], StripChars) ->
    case lists:member(Ch, StripChars) of
        true ->
            case strip_right(S, StripChars) of
                [] -> [];
                T  -> [Ch|T]
            end;
        false ->
            [Ch|strip_right(S, StripChars)]
    end;
strip_right([], _StripChars) ->
    [].

%% @spec is_whitespace(char()) -> bool()
%% @doc Tests if a character is whitespace.
-spec(is_whitespace/1 :: (char()) -> bool()).

is_whitespace(Ch) when ?IS_WHITESPACE_GUARD(Ch) ->
    true;
is_whitespace(_) ->
    false.

%% @spec partition(String, Sep) -> {String, [], []} | {Prefix, Sep, Postfix}
%% @doc Inspired by Python 2.5's str.partition:
%%      partition("foo/bar", "/") = {"foo", "/", "bar"},
%%      partition("foo", "/") = {"foo", "", ""}.
%% From MochiWeb: http://code.google.com/p/mochiweb/
-spec(partition/2 :: (string(), string()) -> {string(), string(), string()}).
partition(String, Sep) ->
    case partition(String, Sep, []) of
        undefined ->
            {String, "", ""};
        Result ->
            Result
    end.

partition("", _Sep, _Acc) ->
    undefined;
partition(S, Sep, Acc) ->
    case partition2(S, Sep) of
        undefined ->
            [C | Rest] = S,
            partition(Rest, Sep, [C | Acc]);
        Rest ->
            {lists:reverse(Acc), Sep, Rest}
    end.

partition2(Rest, "") ->
    Rest;
partition2([C | R1], [C | R2]) ->
    partition2(R1, R2);
partition2(_S, _Sep) ->
    undefined.

