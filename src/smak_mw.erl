%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak middleware utilities.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_mw).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([foldl/2, foldr/2, process/2, noop/0, compose/2]).
-export([wrapper/3]).

-include("smak.hrl").

%% @spec foldl(Ctx::ewgi_context(), L::[ewgi_app()]) -> ewgi_context()
%% @doc Fold over middleware components L, processing each one with
%% process/2.
%% @see foldr/2
-spec foldl(ewgi_context(), [ewgi_app()]) -> ewgi_context().
foldl(Ctx0, L) when ?IS_EWGI_CONTEXT(Ctx0), is_list(L) ->
    lists:foldl(fun process/2, Ctx0, L).

%% @spec foldr(Ctx::ewgi_context(), L::[ewgi_app()]) -> ewgi_context()
%% @doc Same as foldl/2 but in the opposite direction.
%% @see foldl/2
-spec foldr(ewgi_context(), [ewgi_app()]) -> ewgi_context().
foldr(Ctx0, L) when ?IS_EWGI_CONTEXT(Ctx0), is_list(L) ->
    lists:foldr(fun process/2, Ctx0, L).

%% @spec process(F::ewgi_app(), Ctx::ewgi_context()) -> ewgi_context()
%% @doc If the context contains no errors, processes a middleware
%% component.  If the middleware generates an error, it is optionally
%% logged and an error is returned.
-spec process(ewgi_app(), ewgi_context()) -> ewgi_context().
process(F, Ctx) when ?IS_EWGI_CONTEXT(Ctx), ?IS_EWGI_APPLICATION(F) ->
    case ewgi_api:response_error(Ctx) of
        undefined ->
	    F(Ctx);
        _ ->
            Ctx
    end.

%% @spec noop() -> ewgi_app()
%% @doc Simple 'noop' middleware that returns the EWGI context unchanged.
-spec noop() -> ewgi_app().
noop() ->
    fun(Ctx) -> Ctx end.

%% @spec compose(G::ewgi_app(), F::ewgi_app()) -> ewgi_app()
%% @doc Compose two EWGI applications: G &#x2218; F
%% Equivalent to: fun(C) -> G(F(C)) end.
-spec compose(ewgi_app(), ewgi_app()) -> ewgi_app().
compose(G, F) when ?IS_EWGI_APPLICATION(G), ?IS_EWGI_APPLICATION(F)->
    fun(Ctx) -> G(F(Ctx)) end.

%% @spec wrapper(ReqHandler::ewgi_app(),
%%               RespHandler::ewgi_app(),
%%               App::ewgi_app()) -> ewgi_app()
%% @doc Creates a 'wrapper' EWGI application which combines a request
%% handler app (called before the inner application) and a response
%% handler app (called with the result of the inner application).
%% @equiv compose(Resp, compose(App, Req))
wrapper(Req, Resp, App) when ?IS_EWGI_APPLICATION(Req),
                             ?IS_EWGI_APPLICATION(Resp),
                             ?IS_EWGI_APPLICATION(App) ->
    compose(Resp, compose(App, Req)).
