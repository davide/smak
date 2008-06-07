%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2008 Smarkets Limited.
%%
%% @doc Smak HTTP status methods. A middleware application is provided which
%% catches exceptions of a special form and displays the appropriate status
%% message.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/

-module(smak_http_status).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([http_error/1]).

%% HTTP Client Errors
-export([bad_request/3, unauthorized/3, payment_required/3, forbidden/3,
         not_found/3, method_not_allowed/3, not_acceptable/3,
         proxy_authentication_required/3, request_timeout/3, conflict/3,
         gone/3, length_required/3, precondition_failed/3,
         request_entity_too_large/3, request_uri_too_long/3,
         unsupported_media_type/3, request_range_not_satisfiable/3,
         expectation_failed/3]).

%% HTTP Server Errors
-export([internal_server_error/3, not_implemented/3, bad_gateway/3,
         service_unavailable/3, gateway_timeout/3, version_not_supported/3]).

-include("smak.hrl").

-define(DEFAULT_HTML_TMPL, fun(Explanation, Detail, Comment, _) ->
                                   io_lib:format("~s\r\n<br />~s\r\n<!-- ~s -->",
                                                 [Explanation, Detail, Comment])
                           end).

-define(DEFAULT_PLAIN_TMPL, fun(Explanation, Detail, _Comment, _) ->
                                    io_lib:format("~s\r\n~s\r\n",
                                                  [Explanation, Detail])
                            end).

-record(error_template, {
          html = ?DEFAULT_HTML_TMPL,
          plain = ?DEFAULT_PLAIN_TMPL
         }).

-record(client_error, {
          code = undefined,
          title = undefined,
          template = #error_template{}, 
          explanation = [],
          detail = [],
          comment = [],
          headers = [],
          req_headers = []
         }).

%% External API

%% @spec http_error(ewgi_app()) -> ewgi_app()
%% @doc This middleware automatically catches exceptions of the form
-spec(http_error/1 :: (ewgi_app()) -> ewgi_app()).

http_error(Application) ->
    fun(Env, StartResp) ->
            try
                smak_ewgi:call_application(Application, Env, StartResp)
            catch
                {http_error, Code} ->
                    (lookup_error(Code, {[], [], []}))(Env, StartResp);
                  {http_error, Code, Extra} ->
                    (lookup_error(Code, Extra))(Env, StartResp);
                  _ ->
                    (internal_server_error([], [], []))(Env, StartResp)
            end
    end.

lookup_error(400, {Detail, Headers, Comment}) ->
    bad_request(Detail, Headers, Comment);
lookup_error(401, {Detail, Headers, Comment}) ->
    unauthorized(Detail, Headers, Comment);
lookup_error(402, {Detail, Headers, Comment}) ->
    payment_required(Detail, Headers, Comment);
lookup_error(403, {Detail, Headers, Comment}) ->
    forbidden(Detail, Headers, Comment);
lookup_error(404, {Detail, Headers, Comment}) ->
    not_found(Detail, Headers, Comment);
lookup_error(405, {Detail, Headers, Comment}) ->
    method_not_allowed(Detail, Headers, Comment);
lookup_error(406, {Detail, Headers, Comment}) ->
    not_acceptable(Detail, Headers, Comment);
lookup_error(407, {Detail, Headers, Comment}) ->
    proxy_authentication_required(Detail, Headers, Comment);
lookup_error(408, {Detail, Headers, Comment}) ->
    request_timeout(Detail, Headers, Comment);
lookup_error(409, {Detail, Headers, Comment}) ->
    conflict(Detail, Headers, Comment);
lookup_error(410, {Detail, Headers, Comment}) ->
    gone(Detail, Headers, Comment);
lookup_error(411, {Detail, Headers, Comment}) ->
    length_required(Detail, Headers, Comment);
lookup_error(412, {Detail, Headers, Comment}) ->
    precondition_failed(Detail, Headers, Comment);
lookup_error(413, {Detail, Headers, Comment}) ->
    request_entity_too_large(Detail, Headers, Comment);
lookup_error(414, {Detail, Headers, Comment}) ->
    request_uri_too_long(Detail, Headers, Comment);
lookup_error(415, {Detail, Headers, Comment}) ->
    unsupported_media_type(Detail, Headers, Comment);
lookup_error(416, {Detail, Headers, Comment}) ->
    request_range_not_satisfiable(Detail, Headers, Comment);
lookup_error(417, {Detail, Headers, Comment}) ->
    expectation_failed(Detail, Headers, Comment);
lookup_error(500, {Detail, Headers, Comment}) ->
    internal_server_error(Detail, Headers, Comment);
lookup_error(501, {Detail, Headers, Comment}) ->
    not_implemented(Detail, Headers, Comment);
lookup_error(502, {Detail, Headers, Comment}) ->
    bad_gateway(Detail, Headers, Comment);
lookup_error(503, {Detail, Headers, Comment}) ->
    service_unavailable(Detail, Headers, Comment);
lookup_error(504, {Detail, Headers, Comment}) ->
    gateway_timeout(Detail, Headers, Comment);
lookup_error(505, {Detail, Headers, Comment}) ->
    version_not_supported(Detail, Headers, Comment).

%%  Client Error 4xx
%%      400 Bad Request
%%      401 Unauthorized
%%      402 Payment Required
%%      403 Forbidden
%%      404 Not Found
%%      405 Method Not Allowed
%%      406 Not Acceptable
%%      407 Proxy Authentication Required
%%      408 Request Timeout
%%      409 Conflict
%%      410 Gone
%%      411 Length Required
%%      412 Precondition Failed
%%      413 Request Entity Too Large
%%      414 Request-URI Too Long
%%      415 Unsupported Media Type
%%      416 Requested Range Not Satisfiable
%%      417 Expectation Failed

%% @spec http_client_error(client_error()) -> ewgi_app()
%% @doc Returns a WSGI application that displays a 4xx-class HTTP client
%% error.
http_client_error(#client_error{code=Code, title=Title}=E) ->
    fun(Env, StartResp) ->
            {Headers, Content} = prepare_content(Env, E),
            StartResp({Code, Title}, Headers),
            [Content]
    end.

%% @spec bad_request(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 400 Bad Request error.
-spec(bad_request/3 :: (string(), proplist(), string()) -> ewgi_app()).

bad_request(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=400,
                                    title="Bad Request",
                                    explanation="The request could not be understood by the server due to malformed syntax.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec unauthorized(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 401 Unauthorized error.
-spec(unauthorized/3 :: (string(), proplist(), string()) -> ewgi_app()).

unauthorized(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=401,
                                    title="Unauthorized",
                                    explanation="The request requires user authentication.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec payment_required(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 402 Payment Required error. The RFC 2616 says this code is "reserved for future use."
-spec(payment_required/3 :: (string(), proplist(), string()) -> ewgi_app()).

payment_required(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=402,
                                    title="Payment Required",
                                    explanation="This resource requires a payment to access.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec forbidden(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 403 Forbidden error.
-spec(forbidden/3 :: (string(), proplist(), string()) -> ewgi_app()).

forbidden(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=403,
                                    title="Forbidden",
                                    explanation="Access was denied to this resource.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec not_found(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 404 Not Found error.
-spec(not_found/3 :: (string(), proplist(), string()) -> ewgi_app()).

not_found(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=404,
                                    title="Not Found",
                                    explanation="The resource requested could not be found.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec method_not_allowed(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 405 Method Not Allowed error.
-spec(method_not_allowed/3 :: (string(), proplist(), string()) -> ewgi_app()).

method_not_allowed(Detail, Headers, Comment) ->
    Tmpl = fun(_, D, _, E) ->
                   Method = smak_ewgi:request_method(E),
                   io_lib:format("The method ~p is not allowed for this resource.\r\n~s", [Method, D])
           end,
    http_client_error(#client_error{code=405,
                                    title="Method Not Allowed",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment,
                                    template=#error_template{html=Tmpl,
                                                             plain=Tmpl}}).

%% @spec not_acceptable(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 406 Not Acceptable error.
-spec(not_acceptable/3 :: (string(), proplist(), string()) -> ewgi_app()).

not_acceptable(Detail, Headers, Comment) ->
    Tmpl = fun(_, D, _, E) ->
                   HttpAccept = smak_ewgi:http_accept(E),
                   io_lib:format("The resource identified by the request could not generate a "
                                 "response entity with acceptable content characteristics "
                                 "(of type ~s).\r\n~s", [HttpAccept, D])
           end,
    http_client_error(#client_error{code=406,
                                    title="Not Acceptable",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment,
                                    template=#error_template{html=Tmpl,
                                                             plain=Tmpl}}).

%% @spec proxy_authentication_required(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 407 Proxy Authentication Required error.
-spec(proxy_authentication_required/3 :: (string(), proplist(), string()) -> ewgi_app()).

proxy_authentication_required(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=407,
                                    title="Proxy Authentication Required",
                                    explanation="Proxy authentication is required to access this resource.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec request_timeout(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 408 Request Timeout error.
-spec(request_timeout/3 :: (string(), proplist(), string()) -> ewgi_app()).

request_timeout(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=408,
                                    title="Request Timeout",
                                    explanation="The server has waited too long for the client request.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec conflict(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 409 Conflict error.
-spec(conflict/3 :: (string(), proplist(), string()) -> ewgi_app()).

conflict(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=409,
                                    title="Conflict",
                                    explanation="There was a conflict whilst trying to complete your request.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec gone(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 410 Gone error.
-spec(gone/3 :: (string(), proplist(), string()) -> ewgi_app()).

gone(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=410,
                                    title="Gone",
                                    explanation="The resource is no longer available, and there is no forwarding address.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec length_required(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 411 Length Required error.
-spec(length_required/3 :: (string(), proplist(), string()) -> ewgi_app()).

length_required(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=411,
                                    title="Length Required",
                                    explanation="The Content-Length request header was not specified but is required.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec precondition_failed(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 412 Precondition Failed error.
-spec(precondition_failed/3 :: (string(), proplist(), string()) -> ewgi_app()).

precondition_failed(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=412,
                                    title="Precondition Failed",
                                    explanation="Request precondition failed.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec request_entity_too_large(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 413 Request Entity Too Large error.
-spec(request_entity_too_large/3 :: (string(), proplist(), string()) -> ewgi_app()).

request_entity_too_large(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=413,
                                    title="Request Entity Too Large",
                                    explanation="The body of the request was too large.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec request_uri_too_long(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 414 Request-URI Too Long error.
-spec(request_uri_too_long/3 :: (string(), proplist(), string()) -> ewgi_app()).

request_uri_too_long(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=414,
                                    title="Request-URI Too Long",
                                    explanation="The request URI was too long.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec unsupported_media_type(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 415 Unsupported Media Type error.
-spec(unsupported_media_type/3 :: (string(), proplist(), string()) -> ewgi_app()).

unsupported_media_type(Detail, Headers, Comment) ->
    Tmpl = fun(_, D, _, E) ->
                   ContentType = smak_ewgi:content_type(E),
                   io_lib:format("The requested media type ~s is not supported.\r\n~s", [ContentType, D])
           end,
    http_client_error(#client_error{code=415,
                                    title="Unsupported Media Type",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment,
                                    template=#error_template{html=Tmpl,
                                                             plain=Tmpl}}).


%% @spec request_range_not_satisfiable(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 416 Request Range Not Satisfiable error.
-spec(request_range_not_satisfiable/3 :: (string(), proplist(), string()) -> ewgi_app()).

request_range_not_satisfiable(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=416,
                                    title="Request Range Not Satisfiable",
                                    explanation="The Range requested was not available.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec expectation_failed(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 417 Expectation Failed error.
-spec(expectation_failed/3 :: (string(), proplist(), string()) -> ewgi_app()).

expectation_failed(Detail, Headers, Comment) ->
    http_client_error(#client_error{code=417,
                                    title="Expectation Failed",
                                    explanation="Expectation failed.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%%  Server Error 5xx
%%    500 Internal Server Error
%%    501 Not Implemented
%%    502 Bad Gateway
%%    503 Service Unavailable
%%    504 Gateway Timeout
%%    505 HTTP Version Not Supported

%% @spec http_server_error(client_error()) -> ewgi_app()
%% @doc Returns a WSGI application that displays a 5xx-class HTTP server
%% error.
http_server_error(#client_error{code=Code, title=Title}=E) ->
    fun(Env, StartResp) ->
            {Headers, Content} = prepare_content(Env, E),
            StartResp({Code, Title}, Headers),
            [Content]
    end.

%% @spec internal_server_error(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 500 Internal Server error.
-spec(internal_server_error/3 :: (string(), proplist(), string()) -> ewgi_app()).

internal_server_error(Detail, Headers, Comment) ->
    http_server_error(#client_error{code=500,
                                    title="Internal Server Error",
                                    explanation="The server failed to satisfy the request due to an unexpected condition.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec not_implemented(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 501 Not Implemented error.
-spec(not_implemented/3 :: (string(), proplist(), string()) -> ewgi_app()).

not_implemented(Detail, Headers, Comment) ->
    Tmpl = fun(_, D, _, E) ->
                   Method = smak_ewgi:request_method(E),
                   io_lib:format("The request method ~p is not implemented by the server for this resource.\r\n~s", [Method, D])
           end,
    http_server_error(#client_error{code=501,
                                    title="Not Implemented",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment,
                                    template=#error_template{html=Tmpl,
                                                             plain=Tmpl}}).

%% @spec bad_gateway(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 502 Bad Gateway error.
-spec(bad_gateway/3 :: (string(), proplist(), string()) -> ewgi_app()).

bad_gateway(Detail, Headers, Comment) ->
    http_server_error(#client_error{code=502,
                                    title="Bad Gateway",
                                    explanation="Bad gateway.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec service_unavailable(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 503 Service Unavailable error.
-spec(service_unavailable/3 :: (string(), proplist(), string()) -> ewgi_app()).

service_unavailable(Detail, Headers, Comment) ->
    http_server_error(#client_error{code=503,
                                    title="Service Unavailable",
                                    explanation="The server is currently unavailable. Please try again at a later time.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec gateway_timeout(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 504 Gateway Timeout error.
-spec(gateway_timeout/3 :: (string(), proplist(), string()) -> ewgi_app()).

gateway_timeout(Detail, Headers, Comment) ->
    http_server_error(#client_error{code=504,
                                    title="Gateway Timeout",
                                    explanation="The gateway has timed out.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec version_not_supported(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 505 Version Not Supported error.
-spec(version_not_supported/3 :: (string(), proplist(), string()) -> ewgi_app()).

version_not_supported(Detail, Headers, Comment) ->
    http_server_error(#client_error{code=505,
                                    title="Version Not Supported",
                                    explanation="The server does not support that HTTP version.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

prepare_content(Env, Err) ->
    Accept = smak_ewgi:http_accept(Env),
    case Accept of
        undefined ->
            prepare_content2(Accept, 0, Env, Err);
        Accept ->
            prepare_content1(Accept, string:str(Accept, "html"), Env, Err)
    end.

prepare_content1(Accept, 0, Env, Err) ->
    prepare_content2(Accept, string:str(Accept, "*/*"), Env, Err);
prepare_content1(_, _, Env, #client_error{headers=Headers}=Err) ->
    {_, H1} = smak_response:replace_header(Headers, "content-type", "text/html; charset=utf8"),
    C = html_output(Env, Err),
    {H1, C}.

prepare_content2(_, 0, Env, #client_error{headers=Headers}=Err) ->
    {_, H1} = smak_response:replace_header(Headers, "content-type", "text/plain; charset=utf8"),
    C = plain_output(Env, Err),
    {H1, C};
prepare_content2(Accept, N, Env, Err) ->
    prepare_content1(Accept, N, Env, Err).
    
%% @doc Deliver plain text output of the error without escaping.
plain_output(Env, #client_error{code=Code, title=Title, template=#error_template{plain=T}}=Err) ->
    Body = make_body(Env, Err, T),
    io_lib:format("~p ~s\r\n~s\r\n", [Code, Title, Body]).

%% @doc Deliver HTML output of the error with necessary entities escaped.
html_output(Env, #client_error{title=Title, template=#error_template{html=T}}=Err) ->
    Body = make_body(Env,
                     Err#client_error{detail=smak_html_util:escape(Err#client_error.detail),
                                      explanation=smak_html_util:escape(Err#client_error.explanation),
                                      comment=smak_html_util:escape(Err#client_error.comment)},
                     T),
    io_lib:format("<html>\r<head><title>~s</title></head>\r<body>\r<h1>~s</h1>\r"
                  "<p>~s</p><hr noshade=\"noshade\" />\r<div align=\"right\">smak EWGI server</div>\r"
                  "</body>\r</html>\r", [Title, Title, Body]).

%% @doc Create the error body for a particular error and template.
make_body(Env, #client_error{detail=Detail, explanation=Explanation, comment=Comment}, Tmpl) ->
    EscapedEnv = [{K, body_repr(V)} || {K, V} <- Env],
    Tmpl(Detail, Explanation, Comment, EscapedEnv).

body_repr(Str) when is_list(Str) ->
    smak_html_util:escape(Str);
body_repr(A) when is_atom(A) ->
    body_repr(atom_to_list(A));
body_repr(I) when is_integer(I) ->
    body_repr(integer_to_list(I));
body_repr(Bin) when is_binary(Bin) ->
    body_repr(binary_to_list(Bin));
body_repr(V) ->
    V.
    
