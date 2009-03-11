%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak request dispatching.  Once a URL is routed, the
%% dispatcher application selects which view or resource is chosen to
%% handle the request.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_dispatch).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-include_lib("eunit/include/eunit.hrl").

-export([pat_dispatcher/1, tree_dispatcher/2]).

-include("smak.hrl").

%% @spec pat_dispatcher(DispatchFun::fun()) -> ewgi_app()
%% @doc Returns an EWGI dispatcher application which uses DispatchFun
%% to find the correct application to handle the request.
%% 
%% Example:
%% ```
%% App = pat_dispatcher(fun("foo_resource") -> fun foo_resource:ewgi/1;
%%                         ("bar_resource") -> fun bar_resource:ewgi/1;
%%                         (undefined) -> fun error_handler:app_404/1
%%                      end).
%% '''
-spec pat_dispatcher(fun((string()) -> ewgi_app())) -> ewgi_app().
pat_dispatcher(F) when is_function(F, 1) ->
    fun(Ctx0) ->
            Route = ewgi_api:find_data(?ROUTE_KEY, Ctx0),
            (F(Route))(Ctx0)
    end.

%% @spec tree_dispatcher(DispatchTree::gb_tree(),
%%                       NotFound::ewgi_app()) -> ewgi_app()
%% @doc Returns an EWGI dispatcher application which uses DispatchTree
%% to find the correct application to handle the request.  If no
%% application is found, the NotFound application is used.
%% 
%% Example:
%% ```
%% App = tree_dispatcher(
%%         gb_trees:from_orddict([{"foo", fun foo_resource:ewgi/1},
%%                                {"bar", fun bar_resource:ewgi/1}]),
%%         fun error_handler:not_found/1).
%% '''
-spec tree_dispatcher(gb_tree(), ewgi_app()) -> ewgi_app().
tree_dispatcher(Tree, NotFound) when ?IS_EWGI_APPLICATION(NotFound) ->
    fun(Ctx0) ->
            Route = ewgi_api:find_data(?ROUTE_KEY, Ctx0),
            case gb_trees:lookup(Route, Tree) of
                {value, App} ->
                    App(Ctx0);
                none ->
                    NotFound(Ctx0)
            end
    end.
