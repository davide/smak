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

-export([pattern/2, gb_tree/2]).

-include("smak.hrl").

-type dispatch_app() :: fun((ewgi_context(), route_pmatches()) -> ewgi_context()).
%% @type dispatch_app() = function()

%% @spec pattern(DispatchFun::function(),
%%               NotFound::ewgi_app()) -> ewgi_app()
%% @doc Returns an EWGI dispatcher application which uses DispatchFun
%% to find the correct application to handle the request.
%% 
%% Example:
%% ```
%% App = pattern(fun("foo_resource") -> fun foo_resource:ewgi/2;
%%                  ("bar_resource") -> fun bar_resource:ewgi/2
%%               end,
%%               fun error_handler:not_found/1).
%% '''
%% 
%% If the URL matches the route "foo_resource", it is called as:
%% ```
%% foo_resource:ewgi(Ctx, [{arg1, "qux"}, {arg2, "yggdrasil"}])
%% '''
%% 
%% The dispatcher application is similar to a typical EWGI application
%% but has an additional proplists for performance and readability.
%% 
%% The special case of the NotFound route should be a simple EWGI
%% application as the arguments are unncessary.
-spec pattern(fun((string()) -> dispatch_app()),
                        ewgi_app()) -> ewgi_app().
pattern(F, NotFound) when is_function(F, 1) ->
    fun(Ctx0) ->
            case ewgi_api:find_data(?ROUTE_KEY, Ctx0, nomatch) of
                {Name, Args} ->
                    (F(Name))(Ctx0, Args);
                nomatch ->
                    NotFound(Ctx0)
            end
    end.

%% @spec gb_tree(DispatchTree::gb_tree(),
%%               NotFound::ewgi_app()) -> ewgi_app()
%% @doc Returns an EWGI dispatcher application which uses DispatchTree
%% to find the correct application to handle the request.  If no
%% application is found, the NotFound application is used.
%% 
%% Example:
%% ```
%% App = gb_tree(
%%         gb_trees:from_orddict([{"foo", fun foo_resource:ewgi/2},
%%                                {"bar", fun bar_resource:ewgi/2}]),
%%         fun error_handler:not_found/1).
%% '''
%% @see pattern/2
-spec gb_tree(gb_tree(), ewgi_app()) -> ewgi_app().
gb_tree(Tree, NotFound) when ?IS_EWGI_APPLICATION(NotFound) ->
    fun(Ctx0) ->
            case ewgi_api:find_data(?ROUTE_KEY, Ctx0, nomatch) of
                {Name, Args} ->
                    case gb_trees:lookup(Name, Tree) of
                        {value, App} ->
                            App(Ctx0, Args);
                        none ->
                            NotFound(Ctx0)
                    end;
                nomatch ->
                    NotFound(Ctx0)
            end
    end.
