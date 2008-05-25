%% @type property() = atom() | tuple()
-type(property() :: atom() | tuple()).

%% @type proplist() = [property()]
-type(proplist() :: [property()]).

%% @type iodata() = binary() | iolist()
-type(iodata() :: binary() | iolist()).

%% NOTE: this function should always return a streamr()
%% @type stream() = function()
-type(stream() :: function()).

%% @type streamr() = {} | {any(), stream()}
-type(streamr() :: {} | {any(), stream()}).

%% @type ewgi_response() = iodata() | stream()
-type(ewgi_response() :: iodata() | stream()).

%% @type ewgi_app() = function() | module()
-type(ewgi_app() :: function() | atom() | tuple()).

%%
%% HTTP status codes
%%
%% 1xx: Informational
-define(CONTINUE,     {100, "Continue"}).
-define(SWITCH_PROTO, {101, "Switching Protocols"}).

%% 2xx: Success
-define(OK,                {200, "OK"}).
-define(CREATED,           {201, "Created"}).
-define(ACCEPTED,          {202, "Accepted"}).
-define(NON_AUTHORITATIVE, {203, "Non-Authoritative Information"}).
-define(NO_CONTENT,        {204, "No Content"}).
-define(RESET,             {205, "Reset Content"}).
-define(PARTIAL,           {206, "Partial Content"}).

%% 3xx: Redirection
-define(MULTIPLE_CHOICES, {300, "Multiple Choices"}).
-define(MOVED,            {301, "Moved Permanently"}).
-define(FOUND,            {302, "Found"}).
-define(SEE_OTHER,        {303, "See Other"}).
-define(REDIRECT,         {303, "See Other"}). % alias for see_other
-define(NOT_MODIFIED,     {304, "Not Modified"}).
-define(USE_PROXY,        {305, "Use Proxy"}).
-define(TEMPORARY,        {307, "Temporary Redirect"}).

%% 4xx: Client Error
-define(BAD_REQ,                   {400, "Bad Request"}).
-define(UNAUTH,                    {401, "Unauthorized"}).
-define(PAYMENT_REQUIRED,          {402, "Payment Required"}).
-define(FORBIDDEN,                 {403, "Forbidden"}).
-define(NOT_FOUND,                 {404, "Not Found"}).
-define(METHOD_NOT_ALLOWED,        {405, "Method Not Allowed"}).
-define(NOT_ACCEPTABLE,            {406, "Not Acceptable"}).
-define(PROXY_AUTH_REQUIRED,       {407, "Proxy Authentication Required"}).
-define(REQ_TIMEOUT,               {408, "Request Time-out"}).
-define(CONFLICT,                  {409, "Conflict"}).
-define(GONE,                      {410, "Gone"}).
-define(LENGTH_REQUIRED,           {411, "Length Required"}).
-define(PRECONDITION_FAILED,       {412, "Precondition Failed"}).
-define(REQ_ENTITY_TOO_LARGE,      {413, "Request Entity Too Large"}).
-define(REQ_URI_TOO_LARGE,         {414, "Request-URI Too Large"}).
-define(UNSUPPORTED_MEDIA_TYPE,    {415, "Unsupported Media Type"}).
-define(REQ_RANGE_NOT_SATISFIABLE, {416, "Requested range not satisfiable"}).
-define(EXPECTATION_FAILED,        {417, "Expectation Failed"}).

%% 5xx: Server Error
-define(INTERNAL_SERVER_ERROR,      {500, "Internal Server Error"}).
-define(ERROR,                      {500, "Internal Server Error"}). % alias for internal_server_error
-define(NOT_IMPLEMENTED,            {501, "Not Implemented"}).
-define(BAD_GW,                     {502, "Bad Gateway"}).
-define(SERVICE_UNAVAILABLE,        {503, "Service Unavailable"}).
-define(GW_TIMEOUT,                 {504, "Gateway Time-out"}).
-define(HTTP_VERSION_NOT_SUPPORTED, {505, "HTTP Version not supported"}).
