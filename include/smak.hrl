-ifndef(_SMAK_HRL).
-define(_SMAK_HRL, 1).

%% Include EWGI only once
-include("ewgi.hrl").

%% @type property() = atom() | tuple()
-type(property() :: atom() | tuple()).

%% @type proplist() = [property()]
-type(proplist() :: [property()]).

%% @type iodata() = binary() | iolist()
-type(iodata() :: binary() | iolist()).

%% @type ewgi_response() = iodata() | stream()
-type(ewgi_response() :: iodata() | stream()).

%% @type ewgi_env() = proplist()
-type(ewgi_env() :: proplist()).

%% @type ewgi_response_headers() = [{string(), string()}]
-type(ewgi_response_headers() :: [{string(), string()}]).

%% @type ewgi_write() = function()
-type(ewgi_write() :: fun((iodata()) -> ok)).

%% @type ewgi_start_response() = function()
-type(ewgi_start_response() :: fun((ewgi_status(), ewgi_response_headers()) -> ewgi_write())).

%% Record representing each of the parts of a well-formed URL.
-record(urlparts, {
          scheme=[],
          netloc=[],
          path=[],
          params=[],
          qry=[],
          fragment=[]
         }).

-endif.
