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

-include("smak.hrl").

%% @spec foldl(Ctx::ewgi_context(), L::[ewgi_app()]) -> ewgi_context().
%% @doc Fold over middleware components L, processing each one with
%% process/2.
%% @see foldr/2
-spec foldl(#ewgi_context{}, [ewgi_app()]) -> #ewgi_context{}.
foldl(Ctx0, L) when is_record(Ctx0, ewgi_context), is_list(L) ->
    lists:foldl(fun process/2, Ctx0, L).

%% @spec foldr(Ctx::ewgi_context(), L::[ewgi_app()]) -> ewgi_context().
%% @doc Same as foldl/2 but in the opposite direction.
%% @see foldl/2
-spec foldr(#ewgi_context{}, [ewgi_app()]) -> #ewgi_context{}.
foldr(Ctx0, L) when is_record(Ctx0, ewgi_context), is_list(L) ->
    lists:foldr(fun process/2, Ctx0, L).

%% @spec process(F::ewgi_app(), Ctx::ewgi_context()) -> ewgi_context().
%% @doc If the context contains no errors, processes a middleware
%% component.  If the middleware generates an error, it is optionally
%% logged and an error is returned.
-spec process(ewgi_app(), #ewgi_context{}) -> #ewgi_context{}.
process(F, Ctx) when is_record(Ctx, ewgi_context), ?IS_EWGI_APPLICATION(F) ->
    case smak_ewgi:response_error(Ctx) of
        undefined ->
            try
                F(Ctx)
            catch
                _:Err ->
                    ?CTX_LOG(Ctx, error, "Internal server error: ~p~nTrace: ~p", [Err, erlang:get_stacktrace()]),
                    smak_http_response:internal_server_error([], [], [])
            end;
        _ ->
            Ctx
    end.

%% @spec noop() -> ewgi_app().
%% @doc Simple 'noop' middleware that returns the EWGI context unchanged.
-spec noop() -> ewgi_app().
noop() ->
    fun(Ctx) -> Ctx end.

%% @spec compose(G::ewgi_app(), F::ewgi_app()) -> ewgi_app().
%% @doc Compose two EWGI applications: G ∘ F
%% Equivalent to: fun(C) -> G(F(C)) end.
-spec compose(ewgi_app(), ewgi_app()) -> ewgi_app().
compose(G, F) ->
    fun(Ctx) -> G(F(Ctx)) end.
