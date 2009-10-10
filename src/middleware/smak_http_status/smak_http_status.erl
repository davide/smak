%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak HTTP status methods. A middleware application is provided which
%% catches exceptions of a special form and displays the appropriate status
%% message.
%% @end
%%
%% @reference See <a
%% href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html">RFC 2616</a>
%% for details.
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

%% HTTP Success (2xx)
-export([ok/3, created/3, accepted/3, non_auth_info/3, no_content/3,
         reset_content/3, partial_content/3]).

%% HTTP Redirection (3xx)
-export([multiple_choices/4, moved_perm/4, found/4, see_other/4,
         not_modified/4, use_proxy/4, temporary_redir/4]).

%% HTTP Client Errors (4xx)
-export([bad_request/3, unauthorized/3, payment_required/3, forbidden/3,
         not_found/3, method_not_allowed/3, not_acceptable/3,
         proxy_authentication_required/3, request_timeout/3, conflict/3,
         gone/3, length_required/3, precondition_failed/3,
         request_entity_too_large/3, request_uri_too_long/3,
         unsupported_media_type/3, request_range_not_satisfiable/3,
         expectation_failed/3]).

%% HTTP Server Errors (5xx)
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

-record(body_template, {
          html = ?DEFAULT_HTML_TMPL,
          plain = ?DEFAULT_PLAIN_TMPL
         }).

-record(status, {
          empty_body = false,
          code = undefined,
          title = undefined,
          template = #body_template{}, 
          explanation = [],
          detail = [],
          comment = [],
          headers = [],
          req_headers = []
         }).

%% External API

%% @spec http_error(ewgi_app()) -> ewgi_app()
%% @doc This middleware automatically catches exceptions of the form
-spec http_error(ewgi_app()) -> ewgi_app().
http_error(Application) ->
    fun(Ctx) ->
            try
                ewgi_application:run(Application, Ctx)
            catch
                {http_error, Code} ->
                    (lookup_error(Code, {[], [], []}))(Ctx);
                  {http_error, Code, Extra} ->
                    (lookup_error(Code, Extra))(Ctx);
                  _ ->
                    (internal_server_error([], [], []))(Ctx)
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

%%  Successful 2xx
%%      200 OK
%%      201 Created
%%      202 Accepted
%%      203 Non-Authoritative Information
%%      204 No Content
%%      205 Reset Content
%%      206 Partial Content

%% @spec http_success(status()) -> ewgi_app()
%% @doc Returns a WSGI application that displays a 2xx-class HTTP success
%% message.
http_success(#status{code=Code, title=Title}=E) ->
    fun(Ctx) ->
            {Headers, Content} = prepare_content(Ctx, E),
            smak_http_response:init(Ctx, Content, [{status, {Code, Title}},
                                                   {headers, Headers}])
    end.

%% @spec ok(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 200 OK message.
-spec ok(string(), proplist(), string()) -> ewgi_app().
ok(Detail, Headers, Comment) ->
    http_success(#status{code=200,
                         title="OK",
                         detail=Detail,
                         headers=Headers,
                         comment=Comment}).

%% @spec created(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 201 Created message.
-spec created(string(), proplist(), string()) -> ewgi_app().
created(Detail, Headers, Comment) ->
    http_success(#status{code=201,
                         title="Created",
                         detail=Detail,
                         headers=Headers,
                         comment=Comment}).

%% @spec accepted(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 202 Accepted message.
-spec accepted(string(), proplist(), string()) -> ewgi_app().
accepted(Detail, Headers, Comment) ->
    http_success(#status{code=202,
                         title="Accepted",
                         explanation="The request is accepted for processing.",
                         detail=Detail,
                         headers=Headers,
                         comment=Comment}).

%% @spec non_auth_info(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 203 Non-Authoritative Information message.
-spec non_auth_info(string(), proplist(), string()) -> ewgi_app().
non_auth_info(Detail, Headers, Comment) ->
    http_success(#status{code=203,
                         title="Non-Authoritative Information",
                         detail=Detail,
                         headers=Headers,
                         comment=Comment}).

%% @spec no_content(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 204 No Content message.
-spec no_content(string(), proplist(), string()) -> ewgi_app().
no_content(Detail, Headers, Comment) ->
    http_success(#status{code=204,
                         title="No Content",
                         empty_body=true,
                         detail=Detail,
                         headers=Headers,
                         comment=Comment}).

%% @spec reset_content(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 205 Reset Content message.
-spec reset_content(string(), proplist(), string()) -> ewgi_app().
reset_content(Detail, Headers, Comment) ->
    http_success(#status{code=205,
                         title="Reset Content",
                         empty_body=true,
                         detail=Detail,
                         headers=Headers,
                         comment=Comment}).

%% @spec partial_content(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 206 Partial Content message.
-spec partial_content(string(), proplist(), string()) -> ewgi_app().
partial_content(Detail, Headers, Comment) ->
    http_success(#status{code=206,
                         title="Partial Content",
                         detail=Detail,
                         headers=Headers,
                         comment=Comment}).

%%  Redirection 3xx
%%      300 Multiple Choices
%%      301 Moved Permanently
%%      302 Found
%%      303 See Other
%%      304 Not Modified
%%      305 Use Proxy
%%      306 (Unused)
%%      307 Temporary Redirect

%% @spec http_redir(status(), string() | 'true') -> ewgi_app()
%% @doc Returns a WSGI application that causes a 3xx-class HTTP redirect.
%% TODO: This is currently broken... Need to handle location properly.
http_redir(Status, true) ->
    http_redir(Status, []);
http_redir(#status{code=Code, title=Title}=E, _Location) ->
    fun(Ctx) ->
            {Headers, Content} = prepare_content(Ctx, E),
            smak_http_response:init(Ctx, Content, [{status, {Code, Title}},
                                                   {headers, Headers}])
    end.

%% @spec multiple_choices(string(), proplist(), string(), string() | bool()) -> ewgi_app()
%% @doc Returns a 300 Multiple Choices redirection.
-spec multiple_choices(string(), proplist(), string(), string() | bool()) -> ewgi_app().
multiple_choices(Detail, Headers, Comment, Location) ->
    http_redir(#status{code=300,
                       title="Multiple Choices",
                       explanation="The resource has been moved to",
                       detail=Detail,
                       headers=Headers,
                       comment=Comment},
               Location).

%% @spec moved_perm(string(), proplist(), string(), string() | bool()) -> ewgi_app()
%% @doc Returns a 301 Moved Permanently redirection.
-spec moved_perm(string(), proplist(), string(), string() | bool()) -> ewgi_app().
moved_perm(Detail, Headers, Comment, Location) ->
    http_redir(#status{code=301,
                       title="Moved Permanently",
                       explanation="The resource has been moved to",
                       detail=Detail,
                       headers=Headers,
                       comment=Comment},
               Location).

%% @spec found(string(), proplist(), string(), string() | bool()) -> ewgi_app()
%% @doc Returns a 302 Found redirection.
-spec found(string(), proplist(), string(), string() | bool()) -> ewgi_app().
found(Detail, Headers, Comment, Location) ->
    http_redir(#status{code=302,
                       title="Found",
                       explanation="The resource was found at",
                       detail=Detail,
                       headers=Headers,
                       comment=Comment},
               Location).

%% @spec see_other(string(), proplist(), string(), string() | bool()) -> ewgi_app()
%% @doc Returns a 303 See Other redirection. This redirect can safely be used
%% after a POST request (the new location shoult be retrieved with GET by the
%% user agent).
-spec see_other(string(), proplist(), string(), string() | bool()) -> ewgi_app().
see_other(Detail, Headers, Comment, Location) ->
    http_redir(#status{code=303,
                       title="See Other",
                       explanation="The resource has been moved to",
                       detail=Detail,
                       headers=Headers,
                       comment=Comment},
              Location).

%% @spec not_modified(string(), proplist(), string(), string() | bool()) -> ewgi_app()
%% @doc Returns a 304 Not Modified redirection. Note: This should include a
%% Date or ETag header...
-spec not_modified(string(), proplist(), string(), string() | bool()) -> ewgi_app().
not_modified(Detail, Headers, Comment, Location) ->
    http_redir(#status{code=304,
                       title="Not Modified",
                       explanation="The resource has been moved to",
                       empty_body=true,
                       detail=Detail,
                       headers=Headers,
                       comment=Comment},
              Location).

%% @spec use_proxy(string(), proplist(), string(), string() | bool()) -> ewgi_app()
%% @doc Returns a 305 Use Proxy redirection.
-spec use_proxy(string(), proplist(), string(), string() | bool()) -> ewgi_app().
use_proxy(Detail, Headers, Comment, Location) ->
    http_redir(#status{code=305,
                       title="Use Proxy",
                       explanation="The resource must be accessed through the proxy located at",
                       detail=Detail,
                       headers=Headers,
                       comment=Comment},
              Location).

%% @spec temporary_redir(string(), proplist(), string(), string() | bool()) -> ewgi_app()
%% @doc Returns a 307 Temporary Redirect redirection.
-spec temporary_redir(string(), proplist(), string(), string() | bool()) -> ewgi_app().
temporary_redir(Detail, Headers, Comment, Location) ->
    http_redir(#status{code=307,
                       title="Temporary Redirect",
                       explanation="The resource has been moved to",
                       detail=Detail,
                       headers=Headers,
                       comment=Comment},
              Location).

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

%% @spec http_client_error(status()) -> ewgi_app()
%% @doc Returns a WSGI application that displays a 4xx-class HTTP client
%% error.
http_client_error(#status{code=Code, title=Title}=E) ->
    fun(Ctx) ->
            {Headers, Content} = prepare_content(Ctx, E),
            smak_http_response:init(Ctx, Content, [{status, {Code, Title}},
                                                   {headers, Headers}])
    end.

%% @spec bad_request(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 400 Bad Request error.
-spec bad_request(string(), proplist(), string()) -> ewgi_app().
bad_request(Detail, Headers, Comment) ->
    http_client_error(#status{code=400,
                              title="Bad Request",
                              explanation="The request could not be understood by the server due to malformed syntax.",
                              detail=Detail,
                              headers=Headers,
                              comment=Comment}).

%% @spec unauthorized(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 401 Unauthorized error.
-spec unauthorized(string(), proplist(), string()) -> ewgi_app().
unauthorized(Detail, Headers, Comment) ->
    http_client_error(#status{code=401,
                                    title="Unauthorized",
                                    explanation="The request requires user authentication.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec payment_required(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 402 Payment Required error. The RFC 2616 says this code is "reserved for future use."
-spec payment_required(string(), proplist(), string()) -> ewgi_app().
payment_required(Detail, Headers, Comment) ->
    http_client_error(#status{code=402,
                                    title="Payment Required",
                                    explanation="This resource requires a payment to access.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec forbidden(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 403 Forbidden error.
-spec forbidden(string(), proplist(), string()) -> ewgi_app().
forbidden(Detail, Headers, Comment) ->
    http_client_error(#status{code=403,
                                    title="Forbidden",
                                    explanation="Access was denied to this resource.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec not_found(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 404 Not Found error.
-spec not_found(string(), proplist(), string()) -> ewgi_app().
not_found(Detail, Headers, Comment) ->
    http_client_error(#status{code=404,
                                    title="Not Found",
                                    explanation="The resource requested could not be found.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec method_not_allowed(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 405 Method Not Allowed error.
-spec method_not_allowed(string(), proplist(), string()) -> ewgi_app().
method_not_allowed(Detail, Headers, Comment) ->
    Tmpl = fun(_, D, _, E) ->
                   Method = ewgi_api:request_method(E),
                   io_lib:format("The method ~p is not allowed for this resource.\r\n~s", [Method, D])
           end,
    http_client_error(#status{code=405,
                                    title="Method Not Allowed",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment,
                                    template=#body_template{html=Tmpl,
                                                             plain=Tmpl}}).

%% @spec not_acceptable(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 406 Not Acceptable error.
-spec not_acceptable(string(), proplist(), string()) -> ewgi_app().
not_acceptable(Detail, Headers, Comment) ->
    Tmpl = fun(_, D, _, E) ->
                   HttpAccept = ewgi_api:get_header_value("accept", E),
                   io_lib:format("The resource identified by the request could not generate a "
                                 "response entity with acceptable content characteristics "
                                 "(of type ~s).\r\n~s", [HttpAccept, D])
           end,
    http_client_error(#status{code=406,
                                    title="Not Acceptable",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment,
                                    template=#body_template{html=Tmpl,
                                                             plain=Tmpl}}).

%% @spec proxy_authentication_required(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 407 Proxy Authentication Required error.
-spec proxy_authentication_required(string(), proplist(), string()) -> ewgi_app().
proxy_authentication_required(Detail, Headers, Comment) ->
    http_client_error(#status{code=407,
                                    title="Proxy Authentication Required",
                                    explanation="Proxy authentication is required to access this resource.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec request_timeout(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 408 Request Timeout error.
-spec request_timeout(string(), proplist(), string()) -> ewgi_app().
request_timeout(Detail, Headers, Comment) ->
    http_client_error(#status{code=408,
                                    title="Request Timeout",
                                    explanation="The server has waited too long for the client request.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec conflict(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 409 Conflict error.
-spec conflict(string(), proplist(), string()) -> ewgi_app().
conflict(Detail, Headers, Comment) ->
    http_client_error(#status{code=409,
                                    title="Conflict",
                                    explanation="There was a conflict whilst trying to complete your request.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec gone(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 410 Gone error.
-spec gone(string(), proplist(), string()) -> ewgi_app().
gone(Detail, Headers, Comment) ->
    http_client_error(#status{code=410,
                                    title="Gone",
                                    explanation="The resource is no longer available, and there is no forwarding address.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec length_required(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 411 Length Required error.
-spec length_required(string(), proplist(), string()) -> ewgi_app().
length_required(Detail, Headers, Comment) ->
    http_client_error(#status{code=411,
                                    title="Length Required",
                                    explanation="The Content-Length request header was not specified but is required.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec precondition_failed(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 412 Precondition Failed error.
-spec precondition_failed(string(), proplist(), string()) -> ewgi_app().
precondition_failed(Detail, Headers, Comment) ->
    http_client_error(#status{code=412,
                                    title="Precondition Failed",
                                    explanation="Request precondition failed.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec request_entity_too_large(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 413 Request Entity Too Large error.
-spec request_entity_too_large(string(), proplist(), string()) -> ewgi_app().
request_entity_too_large(Detail, Headers, Comment) ->
    http_client_error(#status{code=413,
                                    title="Request Entity Too Large",
                                    explanation="The body of the request was too large.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec request_uri_too_long(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 414 Request-URI Too Long error.
-spec request_uri_too_long(string(), proplist(), string()) -> ewgi_app().
request_uri_too_long(Detail, Headers, Comment) ->
    http_client_error(#status{code=414,
                                    title="Request-URI Too Long",
                                    explanation="The request URI was too long.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec unsupported_media_type(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 415 Unsupported Media Type error.
-spec unsupported_media_type(string(), proplist(), string()) -> ewgi_app().
unsupported_media_type(Detail, Headers, Comment) ->
    Tmpl = fun(_, D, _, E) ->
                   ContentType = ewgi_api:content_type(E),
                   io_lib:format("The requested media type ~s is not supported.\r\n~s", [ContentType, D])
           end,
    http_client_error(#status{code=415,
                                    title="Unsupported Media Type",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment,
                                    template=#body_template{html=Tmpl,
                                                             plain=Tmpl}}).


%% @spec request_range_not_satisfiable(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 416 Request Range Not Satisfiable error.
-spec request_range_not_satisfiable(string(), proplist(), string()) -> ewgi_app().
request_range_not_satisfiable(Detail, Headers, Comment) ->
    http_client_error(#status{code=416,
                                    title="Request Range Not Satisfiable",
                                    explanation="The Range requested was not available.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec expectation_failed(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 417 Expectation Failed error.
-spec expectation_failed(string(), proplist(), string()) -> ewgi_app().
expectation_failed(Detail, Headers, Comment) ->
    http_client_error(#status{code=417,
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
http_server_error(#status{code=Code, title=Title}=E) ->
    fun(Ctx) ->
            {Headers, Content} = prepare_content(Ctx, E),
            smak_http_response:init(Ctx, Content, [{status, {Code, Title}},
                                                   {headers, Headers}])
    end.

%% @spec internal_server_error(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 500 Internal Server error.
-spec internal_server_error(string(), proplist(), string()) -> ewgi_app().
internal_server_error(Detail, Headers, Comment) ->
    http_server_error(#status{code=500,
                                    title="Internal Server Error",
                                    explanation="The server failed to satisfy the request due to an unexpected condition.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec not_implemented(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 501 Not Implemented error.
-spec not_implemented(string(), proplist(), string()) -> ewgi_app().
not_implemented(Detail, Headers, Comment) ->
    Tmpl = fun(_, D, _, E) ->
                   Method = ewgi_api:request_method(E),
                   io_lib:format("The request method ~p is not implemented by the server for this resource.\r\n~s", [Method, D])
           end,
    http_server_error(#status{code=501,
                                    title="Not Implemented",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment,
                                    template=#body_template{html=Tmpl,
                                                             plain=Tmpl}}).

%% @spec bad_gateway(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 502 Bad Gateway error.
-spec bad_gateway(string(), proplist(), string()) -> ewgi_app().
bad_gateway(Detail, Headers, Comment) ->
    http_server_error(#status{code=502,
                                    title="Bad Gateway",
                                    explanation="Bad gateway.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec service_unavailable(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 503 Service Unavailable error.
-spec service_unavailable(string(), proplist(), string()) -> ewgi_app().
service_unavailable(Detail, Headers, Comment) ->
    http_server_error(#status{code=503,
                                    title="Service Unavailable",
                                    explanation="The server is currently unavailable. Please try again at a later time.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec gateway_timeout(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 504 Gateway Timeout error.
-spec gateway_timeout(string(), proplist(), string()) -> ewgi_app().
gateway_timeout(Detail, Headers, Comment) ->
    http_server_error(#status{code=504,
                                    title="Gateway Timeout",
                                    explanation="The gateway has timed out.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

%% @spec version_not_supported(string(), proplist(), string()) -> ewgi_app()
%% @doc Returns a 505 Version Not Supported error.
-spec version_not_supported(string(), proplist(), string()) -> ewgi_app().
version_not_supported(Detail, Headers, Comment) ->
    http_server_error(#status{code=505,
                                    title="Version Not Supported",
                                    explanation="The server does not support that HTTP version.",
                                    detail=Detail,
                                    headers=Headers,
                                    comment=Comment}).

prepare_content(Ctx, Err) ->
    case ewgi_api:get_header_value("accept", Ctx) of
        undefined ->
            prepare_content2(undefined, 0, Ctx, Err);
        Accept ->
            prepare_content1(Accept, string:str(Accept, "html"), Ctx, Err)
    end.

prepare_content1(Accept, 0, Ctx, Err) ->
    prepare_content2(Accept, string:str(Accept, "*/*"), Ctx, Err);
prepare_content1(_, _, Ctx, #status{headers=Headers}=Err) ->
    {_, H1} = smak_http_response:replace_header(Headers, "content-type", "text/html; charset=utf8"),
    C = html_output(Ctx, Err),
    {H1, C}.

prepare_content2(_, 0, Ctx, #status{headers=Headers}=Err) ->
    {_, H1} = smak_http_response:replace_header(Headers, "content-type", "text/plain; charset=utf8"),
    C = plain_output(Ctx, Err),
    {H1, C};
prepare_content2(Accept, N, Ctx, Err) ->
    prepare_content1(Accept, N, Ctx, Err).
    
%% @doc Deliver plain text output of the error without escaping.
plain_output(Ctx, #status{code=Code, title=Title, template=#body_template{plain=T}}=Err) ->
    Body = make_body(Ctx, Err, T),
    io_lib:format("~p ~s\r\n~s\r\n", [Code, Title, Body]).

%% @doc Deliver HTML output of the error with necessary entities escaped.
html_output(Ctx, #status{title=Title, template=#body_template{html=T}}=Err) ->
    Body = make_body(Ctx,
                     Err#status{detail=ewgi_util_html:escape(Err#status.detail),
                                      explanation=ewgi_util_html:escape(Err#status.explanation),
                                      comment=ewgi_util_html:escape(Err#status.comment)},
                     T),
    io_lib:format("<html>\r<head><title>~s</title></head>\r<body>\r<h1>~s</h1>\r"
                  "<p>~s</p><hr noshade=\"noshade\" />\r<div align=\"right\">smak EWGI server</div>\r"
                  "</body>\r</html>\r", [Title, Title, Body]).

%% @doc Create the error body for a particular error and template.
make_body(Ctx, #status{detail=Detail, explanation=Explanation, comment=Comment}, Tmpl) ->
    EscapedCtx = [{K, body_repr(V)} || {K, V} <- ewgi_api:get_all_headers(Ctx)],
    Tmpl(Detail, Explanation, Comment, EscapedCtx).

body_repr(Str) when is_list(Str) ->
    ewgi_util_html:escape(Str);
body_repr(A) when is_atom(A) ->
    body_repr(atom_to_list(A));
body_repr(I) when is_integer(I) ->
    body_repr(integer_to_list(I));
body_repr(Bin) when is_binary(Bin) ->
    body_repr(binary_to_list(Bin));
body_repr(V) ->
    V.
    
