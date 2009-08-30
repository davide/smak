%% @author Emad El-Haraty <emad@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc HTTP Cookie parsing (RFC 2109, RFC 2965) shamelessly ripped off mochiweb.

-module(smak_cookie).
-export([parse_cookie/1, test/0]).

-define(QUOTE, $\").

-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% RFC 2616 separators (called tspecials in RFC 2068)
-define(IS_SEPARATOR(C),
        (C < 32 orelse
         C =:= $\s orelse C =:= $\t orelse
         C =:= $( orelse C =:= $) orelse C =:= $< orelse C =:= $> orelse
         C =:= $@ orelse C =:= $, orelse C =:= $; orelse C =:= $: orelse
         C =:= $\\ orelse C =:= $\" orelse C =:= $/ orelse
         C =:= $[ orelse C =:= $] orelse C =:= $? orelse C =:= $= orelse
         C =:= ${ orelse C =:= $})).

%% @spec parse_cookie(string()) -> [{K::string(), V::string()}]
%% @doc Parse the contents of a Cookie header field, ignoring cookie
%% attributes, and return a simple property list.
parse_cookie("") -> 
    [];
parse_cookie(Cookie) -> 
    parse_cookie(Cookie, []).

%% @spec test() -> ok
%% @doc Run tests for mochiweb_cookies.
test() ->
    parse_cookie_test(),
    ok.

%% Internal API

parse_cookie([], Acc) ->
    lists:reverse(Acc); 
parse_cookie(String, Acc) -> 
    {{Token, Value}, Rest} = read_pair(String),
    Acc1 = case Token of
               "" ->
                   Acc;
               "$" ++ _ ->
                   Acc;
               _ ->
                   [{Token, Value} | Acc]
           end,
    parse_cookie(Rest, Acc1).

read_pair(String) ->
    {Token, Rest} = read_token(skip_whitespace(String)),
    {Value, Rest1} = read_value(skip_whitespace(Rest)),
    {{Token, Value}, skip_past_separator(Rest1)}.

read_value([$= | Value]) ->
    Value1 = skip_whitespace(Value),
    case Value1 of
        [?QUOTE | _] ->
            read_quoted(Value1);
        _ ->
            read_token(Value1)
    end;
read_value(String) ->
    {"", String}.

read_quoted([?QUOTE | String]) ->
    read_quoted(String, []).

read_quoted([], Acc) ->
    {lists:reverse(Acc), []};
read_quoted([?QUOTE | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
read_quoted([$\\, Any | Rest], Acc) ->
    read_quoted(Rest, [Any | Acc]);
read_quoted([C | Rest], Acc) ->
    read_quoted(Rest, [C | Acc]).
    
skip_whitespace(String) ->
    F = fun (C) -> ?IS_WHITESPACE(C) end,
    lists:dropwhile(F, String).

read_token(String) ->
    F = fun (C) -> not ?IS_SEPARATOR(C) end,
    lists:splitwith(F, String).

skip_past_separator([]) ->    
    [];
skip_past_separator([$; | Rest]) ->
    Rest;
skip_past_separator([$, | Rest]) ->
    Rest;
skip_past_separator([_ | Rest]) ->
    skip_past_separator(Rest).

parse_cookie_test() ->
    %% RFC example
    C1 = "$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"; 
    Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
    Shipping=\"FedEx\"; $Path=\"/acme\"",
    [
     {"Customer","WILE_E_COYOTE"},
     {"Part_Number","Rocket_Launcher_0001"},
     {"Shipping","FedEx"}
    ] = parse_cookie(C1),
    %% Potential edge cases
    [{"foo", "x"}] = parse_cookie("foo=\"\\x\""),
    [] = parse_cookie("="),
    [{"foo", ""}, {"bar", ""}] = parse_cookie("  foo ; bar  "),
    [{"foo", ""}, {"bar", ""}] = parse_cookie("foo=;bar="),
    [{"foo", "\";"}, {"bar", ""}] = parse_cookie("foo = \"\\\";\";bar "),
    [{"foo", "\";bar"}] = parse_cookie("foo=\"\\\";bar").
