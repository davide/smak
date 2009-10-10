%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Basic Smak HTTP method override middleware.  Parses the
%% x-http-method-override header of a POST request or the _method
%% query-string variable.
%%
%% Example:
%% <pre>
%% OverloadApp = smak_method_overload:init([disable_header]),
%% EwgiApp = smak_mw:compose(OverloadApp, MyApp)
%% </pre>
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/

-module(smak_method_overload).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-include("smak.hrl").

-export([init/0, init/1]).

-type methods() :: [ewgi_request_method()].
-type mo_option() :: 'disable_header' | 'disable_qs' | {'methods_allowed', methods()} | {'qs_key', string()}.

-record(mo_options, {
          disable_header = false :: boolean(),
          disable_qs     = false :: boolean(),
          methods_allowed = default :: 'default' | methods(),
          qs_key = "_method" :: string()
         }).

-spec init() -> ewgi_app().
init() ->
    init([]).

-spec init([mo_option()]) -> ewgi_app().
init(Options0) ->
    Options = lists:foldl(fun parse_option/2, #mo_options{}, Options0),
    HeaderApp = case Options#mo_options.disable_header of
                    false ->
                        header_app(Options#mo_options.methods_allowed);
                    true ->
                        smak_mw:noop()
                end,
    QsApp = case Options#mo_options.disable_qs of
                false ->
                    qs_app(Options#mo_options.methods_allowed,
                           Options#mo_options.qs_key);
                true ->
                    smak_mw:noop()
            end,
    smak_mw:compose(QsApp, HeaderApp).

%%----------------------------------------------------------------------
%% Internal
%%----------------------------------------------------------------------

-spec header_app(methods()) -> ewgi_app().
header_app(Methods) ->
    F = fun(Ctx0) ->
                case ewgi_api:request_method(Ctx0) of
                    'POST' ->
                        header_app1(Methods, Ctx0);
                    _ ->
                        Ctx0
                end
        end,
    F.

-spec header_app1(methods(), ewgi_context()) -> ewgi_context().
header_app1(Methods, Ctx0) ->
    case ewgi_api:get_header_value("x-http-method-override", Ctx0) of
        undefined ->
            Ctx0;
        Header ->
            case override_method(Methods, Header) of
                unchanged ->
                    Ctx0;
                NewMethod ->
                    Ctx = ewgi_api:request_method(NewMethod, Ctx0),
                    Ctx
            end
    end.

-spec qs_app(methods(), string()) -> ewgi_app().
qs_app(Methods, Key) ->    
    F = fun(Ctx0) ->
                case ewgi_api:request_method(Ctx0) of
                    'POST' ->
                        qs_app1(Methods, Key, Ctx0);
                    _ ->
                        Ctx0
                end
        end,
    F.

-spec qs_app1(methods(), string(), ewgi_context()) -> ewgi_context().
qs_app1(Methods, Key, Ctx0) ->
    case ewgi_api:query_string(Ctx0) of
        undefined ->
            Ctx0;
        Qs ->
            case proplists:get_value(Key, ewgi_api:parse_qs(Qs)) of
                undefined ->
                    Ctx0;
                Header ->
                    case override_method(Methods, Header) of
                        unchanged ->
                            Ctx0;
                        NewMethod ->
                            Ctx = ewgi_api:request_method(NewMethod, Ctx0),
                            Ctx
                    end
            end
    end.
    
-spec override_method('default' | methods(), string()) -> ewgi_request_method() | 'unchanged'.
override_method(default, "PUT") ->
    'PUT';
override_method(default, "DELETE") ->
    'DELETE';
override_method(Methods, Method) when is_list(Methods) ->
    case lists:member(Method, Methods) of
        true ->
            override_method_atom(Method);
        false ->
            unchanged
    end;
override_method(_, _) ->
    unchanged.

-spec override_method_atom(string()) -> ewgi_request_method().
override_method_atom("PUT") ->
    'PUT';
override_method_atom("DELETE") ->
    'DELETE';
override_method_atom("POST") ->
    'POST';
override_method_atom("GET") ->
    'GET';
override_method_atom("OPTIONS") ->
    'OPTIONS';
override_method_atom("HEAD") ->
    'HEAD';
override_method_atom("TRACE") ->
    'TRACE';
override_method_atom("CONNECT") ->
    'CONNECT';
override_method_atom(Other) when is_list(Other) ->
    Other.

-spec parse_option(mo_option(), #mo_options{}) -> #mo_options{}.
parse_option(disable_header, M) ->
    M#mo_options{disable_header=true};
parse_option(disable_qs, M) ->
    M#mo_options{disable_qs=true};
parse_option({methods_allowed, Methods}, M) when is_list(Methods) ->
    M#mo_options{methods_allowed=Methods};
parse_option({qs_key, Key}, M) when is_list(Key) ->
    M#mo_options{qs_key=Key}.
