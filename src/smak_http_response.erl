%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak HTTP response utility functions for dealing with EWGI
%% context returned from an application.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_http_response).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([init/2, init/3, replace_header/3]).

-include("smak.hrl").

-include_lib("eunit/include/eunit.hrl").

-type response_key() :: 'status' | 'content_type' | 'headers'.
-type init_params() :: [{response_key(), string() | ewgi_status()}].

% TODO: Most frameworks let the author set this...
-define(DEFAULT_CONTENT_TYPE, "text/plain; charset=utf-8").

% Possible parameters
-record(p, {
          status={200, "OK"} :: ewgi_status(),
          content_type :: string(),
          headers=[] :: ewgi_header_list()
         }).

-spec init(#ewgi_context{}, iolist()) -> #ewgi_context{}.
init(Ctx, Content) ->
    init(Ctx, Content, []).

-spec init(#ewgi_context{}, iolist(), init_params()) -> #ewgi_context{}.
init(Ctx, Content, Params0) ->
    Params = parse(Params0),
    Ctx1 = ewgi_api:response_message_body(Content, Ctx),
    Ctx2 = ewgi_api:response_status(Params#p.status, Ctx1),
    Ctx3 = merge_headers(Params#p.headers, Ctx2),
    add_content_type(Params, Ctx3).

-spec add_content_type(#p{}, #ewgi_context{}) -> #ewgi_context{}.
add_content_type(#p{content_type=undefined}, Ctx) ->
    Hdr0 = ewgi_api:response_headers(Ctx),
    case proplists:get_value("content-type", Hdr0) of
        undefined ->
            Hdr = [{"content-type", ?DEFAULT_CONTENT_TYPE}|Hdr0],
            ewgi_api:response_headers(Hdr, Ctx);
        _ ->
            Ctx
    end;
add_content_type(#p{content_type=V}, Ctx) ->
    Hdr0 = ewgi_api:response_headers(Ctx),
    Hdr = [{"content-type", V}|proplists:delete("content-type", Hdr0)],
    ewgi_api:response_headers(Hdr, Ctx).

-spec merge_headers(ewgi_header_list(), #ewgi_context{}) -> #ewgi_context{}.
merge_headers(L, Ctx) ->
    Hdr0 = ewgi_api:response_headers(Ctx),
    Hdr = lists:foldl(fun({H, V}=A, Headers) ->
                              case proplists:get_value(H, Headers) of
                                  undefined ->
                                      [A|Headers];
                                  V ->
                                      Headers;
                                  _ ->
                                      [A|proplists:delete(H, Headers)]
                              end
                      end, Hdr0, L),
    ewgi_api:response_headers(Hdr, Ctx).

-spec parse(init_params()) -> #p{}.
parse(L) ->
    lists:foldl(fun({status, {S, M}=V}, P) when is_integer(S),
                                                is_list(M) ->
                        P#p{status = V};
                   ({content_type, V}, P) when is_list(V) ->
                        P#p{content_type = V};
                   ({headers, Hl}, P) when is_list(Hl) ->
                        P#p{headers = Hl};
                   (Unk, _) -> 
                        throw({error, {unknown_param, Unk}})
                end, #p{}, L).

%% @spec replace_header(proplist(), string(), string()) -> {string(), proplist()}
%% @doc Replace the first occurrence of the header specified with the
%% value. If the header does not exist, add a new one. Note that this method
%% should not be used for header names such as "set-cookie" which may appear
%% more than once in the headers. Returns the new header list and the
%% previous value (if exists).
-spec replace_header(ewgi_header_list(), string(), string()) -> {string(), ewgi_header_list()}.
replace_header(Headers, Name, Value) ->
    Name1 = string:to_lower(Name),
    replace_header1(Headers, Name1, Value, [], []).

replace_header1([], Name, Value, [], Acc) ->
    {[], lists:reverse([{Name, Value}|Acc])};
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

%%----------------------------------------------------------------------
%% Unit tests
%%----------------------------------------------------------------------

replace_header_test_() ->
    [%% Insert if not found
     ?_assertEqual({[], [{"host", "localhost"}]},
		   replace_header([], "Host", "localhost")),
     %% Replace one occurance (and return old value)
     ?_assertEqual({"localhost", [{"host", "remote"}]},
		   replace_header([{"host", "localhost"}], "Host", "remote")),
     %% Replace first of multiple occurances (and return first old value)
     %% (this function is not suited for this though)
     ?_assertEqual({"localhost", [{"host", "remote"}, {"host", "www"}]},
		   replace_header([{"host", "localhost"}, {"host", "www"}], "Host", "remote"))
    ].
