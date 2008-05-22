%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.

%% @doc Smak miscellaneous string functions

-module(smak_string).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([split/3, is_whitespace/1, strip/1, strip/2, strip/3]).

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
        
%% @spec split(String::string(), Separators::char() | string(),
%%       MaxSplit::integer()) -> [string()]
%% @doc Return a list of strings separated by characters in Separators up to
%%      MaxSplit splits.
-spec(split/3 :: (string(), string(), integer()) -> [[char(),...]]).

split([], [], _N) ->
    [];
split([], _Seps, _N) ->
    [[]];
split(Str, _Seps, 0) ->
    Str;
split(Str, Seps, N) when is_list(Seps) ->
    split1(Str, Seps, N, []).

split1(S, _Seps, 0, Toks) ->
    lists:reverse(Toks) ++ [S];
split1([C|S], Seps, N, Toks) ->
    case lists:member(C, Seps) of
        true -> split1(S, Seps, N-1, Toks);
        false -> split2(S, Seps, N, Toks, [C])
    end;
split1([], _Seps, _N, Toks) ->
    lists:reverse(Toks).

split2([C|S], Seps, N, Toks, Cs) ->
    case lists:member(C, Seps) of
        true -> split1(S, Seps, N-1, [lists:reverse(Cs)|Toks]);
        false -> split2(S, Seps, N, Toks, [C|Cs])
    end;
split2([], _Seps, _N, Toks, Cs) ->
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
