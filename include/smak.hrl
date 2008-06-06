%% @type property() = atom() | tuple()
-type(property() :: atom() | tuple()).

%% @type proplist() = [property()]
-type(proplist() :: [property()]).

%% @type iodata() = binary() | iolist()
-type(iodata() :: binary() | iolist()).

%% @type stream() = function()
%% NOTE: function() should actually be stream() but it appears to break
%% dialyzer at the time of writing.
-type(stream() :: fun(() -> {} | {any(), function()})).

%% @type ewgi_response() = iodata() | stream()
-type(ewgi_response() :: iodata() | stream()).

%% @type ewgi_env() = proplist()
-type(ewgi_env() :: proplist()).

%% @type ewgi_status() = {integer(), string()}
-type(ewgi_status() :: {integer(), string()}).

%% @type ewgi_response_headers() = [{string(), string()}]
-type(ewgi_response_headers() :: [{string(), string()}]).

%% @type ewgi_write() = function()
-type(ewgi_write() :: fun((iodata()) -> ok)).

%% @type ewgi_start_response() = function()
-type(ewgi_start_response() :: fun((ewgi_status(), ewgi_response_headers()) -> ewgi_write())).

%% @type ewgi_app() = function() | module()
-type(ewgi_app() :: fun((ewgi_env(), ewgi_start_response()) -> ewgi_response()) | atom() | tuple()).
