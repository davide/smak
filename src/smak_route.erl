%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak URL routing.  We define a mechanism for building URL routes
%% using regular expressions with the possibility of a naive reverse
%% match which produces a URL from a set of matches.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_route).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([routes_all/0, routes/1]).
-export([resolve/2, reverse/2]).
-export([route/2, route/3, route/4]).
-export([test/0]).

-type regex() :: string().
-type grpidx() :: atom() | integer().
-type mgroup() :: {grpidx(), regex()} | {grpidx(), regex(), any()}.
-type pcond() :: regex() | mgroup().
-type pattern() :: [pcond()].
-type rname() :: string().
-type pmatch() :: {atom(), string()}.
-type pmatches() :: [pmatch()].

-record(route, {
          pattern :: pattern(),
          doc :: binary(),
          subs=[] :: [grpidx()],
          name :: rname()
         }).

-record(croute, {
          route :: #route{},
          defaults=gb_trees:empty() :: tuple(), % gb_tree()
          regex=[] :: any() %% compiled regex
         }).

%% ["users/", {name, "\w+", "foo"}, "/"]
%% gets mapped to a regular expression that looks like:
%%     "^users/(?P<name>\w+)/$"
%%
%% ["users/", {name, "\w+"}, "/images"]
%% becomes:
%%     "^users/(?P<name>\w+)/images$"

-spec routes([#croute{}]) -> tuple().
routes(L) ->
    lists:foldl(fun(#croute{route=#route{name=K}}=V, T) ->
                        gb_trees:insert(K, V, T)
                end, gb_trees:empty(), L).

-spec routes_all() -> tuple().
routes_all() ->
    routes(lists:flatten([look_mod(M) || {M, _} <- code:all_loaded()])).

-spec look_mod(atom()) -> [#croute{}].
look_mod(M) ->
    Exports = proplists:get_value(exports, M:module_info()),
    case proplists:get_value(smak_routes, Exports) of
        0 ->
            try
                M:smak_routes()
            catch
                _:_ ->
                    []
            end;
        _ ->
            []
    end.

-spec route(rname(), pattern()) -> #croute{}.
route(Name, Pat) ->
    route(Name, <<"">>, Pat).

-spec route(rname(), binary(), pattern()) -> #croute{}.
route(Name, Doc, Pat) ->
    route(Name, Doc, Pat, []).

-spec route(rname(), binary(), pattern(), [grpidx()]) -> #croute{}.
route(Name, Doc, Pat, G) ->
    Route = #route{pattern=Pat, name=Name, subs=G, doc=Doc},
    C0 = lists:foldl(fun compile_re/2, #croute{route=Route}, Pat),
    Exp = lists:reverse(lists:flatten(C0#croute.regex)),
    {ok, Exp1} = re:compile(Exp),
    C0#croute{regex=Exp1}.

-spec compile_re(mgroup() | regex(), #croute{}) -> #croute{}.
compile_re({Name, P}, #croute{regex=R}=C) ->
    Exp = "(?P<" ++ rename(Name) ++ ">" ++ P ++ ")",
    C#croute{regex=[lists:reverse(Exp)|R]};
compile_re({Name, P, Default}, #croute{defaults=D}=C0) ->
    C = compile_re({Name, P}, C0),
    C#croute{defaults=gb_trees:insert(Name, Default, D)};
compile_re(P, #croute{regex=R}=C) ->
    C#croute{regex=[lists:reverse(P)|R]}.

-spec rename(binary() | atom() | list() | integer()) -> list().
rename(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
rename(A) when is_atom(A) ->
    atom_to_list(A);
rename(I) when is_integer(I) ->
    integer_to_list(I);
rename(L) when is_list(L) ->
    L.

-type mresult() :: 'nomatch' | {rname(), pmatches()}.
-spec resolve(tuple(), string()) -> mresult().
resolve(T, Url) ->
    resolve(gb_trees:to_list(T), Url, nomatch).

-spec resolve([{rname(), #croute{}}], string(), mresult()) -> mresult().
resolve([{N, H}|T], Url, nomatch) ->
    case resolve1(H, Url) of
        nomatch ->
            resolve(T, Url, nomatch);
        L ->
            {N, L}
    end;
resolve(_, _, Acc) ->
    Acc.

-spec resolve1(#croute{}, string()) -> 'nomatch' | pmatches().
resolve1(#croute{route=#route{subs=S}, regex=R, defaults=D}, Url) ->
    case re:run(Url, R, [{capture, S, list}]) of
        {match, L} when is_list(L) ->
            [resolve_default(I, D) || I <- lists:zip(S, L)];
        nomatch ->
            nomatch;
        match ->
            []
    end.

-spec resolve_default(pmatch(), tuple()) -> pmatch().
resolve_default({Name, []}=Orig, D) ->
    case gb_trees:lookup(Name, D) of
        {value, V} ->
            {Name, V};
        none ->
            Orig
    end;
resolve_default(Orig, _) ->
    Orig.

%% Naive reverse matching.  Ignores type of incoming data against pattern.
-spec reverse(tuple(), {rname(), pmatches()}) -> string() | 'nomatch'.
reverse(T, {N, L}) ->
    case gb_trees:lookup(N, T) of
        none ->
            nomatch;
        {value, C} ->
            reverse1(C, L)
    end.

-spec reverse1(#croute{}, pmatches()) -> string() | {'error', key_not_found, atom()}.
reverse1(#croute{route=#route{pattern=P}}, L) ->
    reverse1(P, L, []).

reverse1([], _, Acc) ->
    lists:flatten(lists:reverse(Acc));
reverse1([H|T], L, Acc) when is_list(H) ->
    reverse1(T, L, [H|Acc]);
reverse1([{Name, _, Default}|T], L, Acc) ->
    reverse1(T, L, [proplists:get_value(Name, L, Default)|Acc]);
reverse1([{Name, _}|T], L, Acc) ->
    case proplists:get_value(Name, L) of
        undefined ->
            {error, key_not_found, Name};
        V ->
            reverse1(T, L, [V|Acc])
    end.

-spec test() -> string().
test() ->
    R0 = route("foo", <<"">>, ["/", {1, "url"}, "/", {bar, "\\w+", "bar"}, "/", {baz, "\\d+", "0"}, "/"], [1, bar, baz]),
    R1 = route("baz", <<"">>, ["/", {1, "static"}, "/"], []),
    Routes = routes([R0, R1]),
    Url = "/url/test/100/",
    M = resolve(Routes, Url),
    Url = reverse(Routes, M).
