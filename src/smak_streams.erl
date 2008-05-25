%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.

%% @doc Smak "streams" implementation for compatibility with EWGI. It makes
%% extensive use of the DELAY and FORCE macros to make the code slightly more
%% readable. Of course, it probably really only comes down to a matter of
%% personal taste. I'm sure some people will get irritated by
%% it. Unfortunately, the BEAM compiler doesn't have any special
%% optimizations for laziness, so this is not a useful module for general
%% functional programming problems. It can probably be used to express
%% certain solutions elegantly, but they will likely suffer severe
%% performance penalties.
%% 
%% Some portions are from Richard Carlsson's 2000 mailing list post which is
%% covered by the LGPL license.
%% http://www.erlang.org/pipermail/erlang-questions/2000-October/001940.html

-module(smak_streams).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([append/2, constant/1, drop/2, duplicate/2, empty/0, filter/2,
	 first/2, foldl/3, foldr/3, foreach/2, integers/1, map/2, member/2,
	 merge/3, nth/2, nthtail/2, push/2, seq/1, seq/2, seq/3,
	 list_to_stream/1, subsequence/3, to_list/1]).

-include("smak.hrl").

-define(DELAY(A), fun() -> A end).
-define(FORCE(S), S()).
-define(HEAD(S), element(1, S)).
-define(TAIL(S), element(2, S)).

%% @spec empty() -> {}
%% @doc Returns an empty stream
-spec(empty/0 :: () -> {}).

empty() ->
    ?DELAY({}).

%% @spec push(any(), stream()) -> stream()
%% @doc Pushes H onto the head of T
-spec(push/2 :: (any(), stream()) -> stream()).

push(H, T) when is_function(T, 0) ->
    ?DELAY({H, T}).

%% @spec to_list(stream()) -> list()
%% @doc Converts a stream into a list. Note that it will never return if an
%% infinite stream is passed to this function. It also has the effect of
%% forcing every lazy element of the stream.
-spec(to_list/1 :: (stream()) -> list()).
     
to_list(S) when is_function(S, 0) ->
    case ?FORCE(S) of
        {H, T} ->
	    [H|to_list(T)];    
	{} ->
	    []
    end.

%% @spec list_to_stream(list()) -> stream()
%% @doc Turns a proper normal list into a stream.
-spec(list_to_stream/1 :: (list()) -> stream()).

list_to_stream(L) when is_list(L) ->
    ?DELAY(case L of
               [H|T] ->
                   {H, list_to_stream(T)};
               [] ->
                   {}
           end).

%% @spec append(stream(), stream()) -> stream()
%% @doc Append the stream S2 to S1, returning a new stream.
-spec(append/2 :: (stream(), stream()) -> stream()).

append(S1, S2) when is_function(S1, 0), is_function(S2, 0) ->
    ?DELAY(case ?FORCE(S1) of
               {} ->
                   ?FORCE(S2);
               {H, T} ->
                   {H, append(T, S2)}
           end).

%% @spec seq(integer()) -> stream()
%% @doc Returns an *infinite* stream starting at From in ascending order.
-spec(seq/1 :: (integer()) -> stream()).

seq(From) when is_integer(From) ->
    ?DELAY({From, seq(From + 1)}).

%% @spec seq(integer(), integer()) -> stream()
%% @doc Returns a stream consisting of integers in the interval From..To in ascending order.
-spec(seq/2 :: (integer(), integer()) -> stream()).

seq(From, To) when is_integer(From), is_integer(To) ->
    seq(From, To, 1).

%% @spec seq(integer(), integer(), integer()) -> stream()
%% @doc The stream of integers [From, From + D, From + 2*D, ..., From +
%% D*((To - From) mod D)]. The interval is empty if D does not have the same
%% sign as the difference To - From.
-spec(seq/3 :: (integer(), integer(), integer()) -> stream()).

seq(From, To, D) when is_integer(From), is_integer(To), From < To, D > 0 ->
    ?DELAY({From, seq(From + D, To, D)});
seq(From, To, D) when is_integer(From), is_integer(To), To < From, D < 0 ->
    ?DELAY({From, seq(From + D, To, D)});
seq(From, To, _D) when is_integer(From), is_integer(To), To =:= From ->
    ?DELAY({From, empty()});
seq(_From, _To, _D) ->
    empty().

%% @spec integers(integer()) -> stream()
%% @doc The stream of integers starting at N.
%% @see seq/1
-spec(integers/1 :: (integer()) -> stream()).

integers(N) when is_integer(N) ->
    seq(N).

%% @spec constant(any()) -> stream()
%% @doc The infinite stream of elements X. 
-spec(constant/1 :: (any()) -> stream()).

constant(X) ->
    ?DELAY({X, constant(X)}).

%% @spec duplicate(integer(), any()) -> stream()
%% @doc The stream of length N, where each element is X.
-spec(duplicate/2 :: (integer(), any()) -> stream()).

duplicate(N, X) when is_integer(N) ->
    ?DELAY(if N > 0 ->
                   {X, duplicate(N - 1, X)};
              N =:= 0 ->
                   {}
           end).

%% @spec first(integer(), stream()) -> stream()
%% @doc The stream consisting of the first N elements of S, or the stream S
%% if the length of S is not greater than N.
-spec(first/2 :: (integer(), stream()) -> stream()).

first(0, _S) ->
    empty();
first(N, S) when is_integer(N), N > 0, is_function(S, 0) ->
    ?DELAY(case ?FORCE(S) of
               {H, T} ->
                   {H, first(N - 1, T)};
               {} ->
                   {}
           end).

%% @spec drop(integer(), stream()) -> stream()
%% @doc The stream [EN+1, EN+2, ...], if S is [E1, ..., EN, EN+1, ...].
-spec(drop/2 :: (integer(), stream()) -> stream()).

drop(0, S) when is_function(S, 0) ->
    S;
drop(N, S) when is_integer(N), N > 0, is_function(S, 0) ->
    ?DELAY(case ?FORCE(S) of
               {_, T} ->
                   ?FORCE((drop(N - 1, T)));
               {} ->
                   {}
           end).

%% @spec subsequence(integer(), integer(), stream()) -> stream()
%% @doc The stream [EN, EN+1, ..., EN+D], if S is [E1, E2, ...].
-spec(subsequence/3 :: (integer(), integer(), stream()) -> stream()).

subsequence(N, D, S) when N > 0, D >= 0 ->
    first(D, drop(N, S)).

%% @spec map(function(), stream()) -> stream()
%% @doc The stream [F(E1), F(E2), F(E3), ...] if S is [E1, E2, E3, ...].
-spec(map/2 :: (function(), stream()) -> stream()).

map(F, S) when is_function(F, 1), is_function(S, 0) ->
    ?DELAY(case ?FORCE(S) of
               {H, T} ->
                   {F(H), map(F, T)};
               {} ->
                   {}
           end).

%% @spec filter(function(), stream()) -> stream()
%% @doc The stream of all elements E in S (in the same order) for which P(E)
%% returns `true'. P must return either `true' or `false' for all elements in
%% S.
-spec(filter/2 :: (function(), stream()) -> stream()).

filter(P, S) when is_function(P, 1), is_function(S, 0) ->
    ?DELAY(case ?FORCE(S) of
               {H, T} ->
                   case P(H) of
                       true ->
                           {H, filter(P, T)};
                       false ->
                           ?FORCE((filter(P, T)))
                   end;
               {} ->
                   {}
           end).

%% @spec merge(function(), function(), function()) -> stream()
%% @doc Returns the stream of elements in S1 and S2 where the respective
%% relative order of elements is preserved, and each element Y of S2 is
%% ordered before the first possible X of S1 such that P(X, Y) yields
%% `false'. P(X, Y) must yield either `true' or `false' for all X in S1 and Y
%% in S2. (P can be read as "less than".)
-spec(merge/3 :: (function(), function(), function()) -> stream()).

merge(P, S1, S2) when is_function(P, 1), is_function(S1, 0), is_function(S2, 0) ->
    ?DELAY(case ?FORCE(S1) of
               {H1, T1} ->
                   case ?FORCE(S2) of
                       {H2, T2} ->
                           case P(H1, H2) of
                               true ->
                                   {H1, merge(P, T1, push(H2, T2))};
                               false ->
                                   {H2, merge(P, push(H1, T1), T2)}
                           end;
                       {} ->
                           {H1, T1}
                   end;
               {} ->
                   ?FORCE(S2)
           end).

%% NOTE: These functions below may force all or part of their input.

%% @spec member(any(), stream()) -> bool()
%% @doc Returns `true' if X is in the stream S, and `false' otherwise.
-spec(member/2 :: (any(), stream()) -> bool()).

member(X, S) when is_function(S, 0) ->
    case ?FORCE(S) of
	{H, T} ->
	    if H == X -> true;
	       true -> member(X, T)
	    end;
	{} ->
            false
    end.

%% @spec nth(integer(), stream()) -> any()
%% @doc Returns the Nth element of stream S.
-spec(nth/2 :: (integer(), stream()) -> any()).

nth(N, S) when N > 1, is_function(S, 0) ->
    nth(N - 1, ?TAIL(?FORCE(S)));
nth(_, S) when is_function(S, 0) ->
    ?HEAD(?FORCE(S)).

%% @spec nthtail(integer(), stream()) -> stream()
%% @doc Returns the stream [AN+1, AN+2, ...] if S is [A1, A2, ..., AN, AN+1,
%% AN+2, ...].
-spec(nthtail/2 :: (integer(), stream()) -> stream()).

nthtail(N, S) when N > 1, is_function(S, 0) ->
    nthtail(N - 1, ?TAIL(?FORCE(S)));
nthtail(0, S) when is_function(S, 0) ->
    S.

%% @spec foldl(any(), function(), stream()) -> any()
%% @doc Computes (...((A F E1) F E2)... F EN), if S is [E1, E2, ..., EN] and
%% F is a binary function (here written as an infix operator).
-spec(foldl/3 :: (any(), function(), stream()) -> any()).

foldl(A, F, S) when is_function(F, 2), is_function(S, 0) ->
    case ?FORCE(S) of
        {H, T} ->
            foldl(F(A, H), F, T);
        {} ->
            A
    end.

%% @spec foldr(stream(), function(), any()) -> any()
%% @doc Computes (E1 F ...(EN-1 F (EN F A))...), if S is [E1, E2, ..., EN]
%% and F is a binary function (here written as an infix operator).
-spec(foldr/3 :: (stream(), function(), any()) -> any()).

foldr(S, F, A) when is_function(S, 0), is_function(F, 2) ->
    case ?FORCE(S) of
	{H, T} ->
	    F(H, foldr(T, F, A));
	{} ->
	    A
    end.

%% @spec foreach(function(), stream()) -> ok
%% @doc Evaluates F(E1), F(E2), ..., if S is [E1, E2, ...]. Always returns
%% `ok'.
-spec(foreach/2 :: (function(), stream()) -> ok).

foreach(F, S) when is_function(F, 1), is_function(S, 0) ->
    case ?FORCE(S) of
	{H, T} ->
	    F(H),
	    foreach(F, T);
	{} ->
	    ok
    end.
