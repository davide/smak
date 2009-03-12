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

-define(IS_EWGI_APPLICATION(A), is_function(A, 1)).

%% Record representing each of the parts of a well-formed URL.
-record(urlparts, {
          scheme=[],
          netloc=[],
          path=[],
          params=[],
          qry=[],
          fragment=[]
         }).

%%----------------------------------------------------------------------
%% Calendar types (from calendar.erl in OTP R12B5)
%%----------------------------------------------------------------------

-type year()     :: non_neg_integer().
-type year1970() :: 1970..10000.	% should probably be 1970..
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.
-type daynum()   :: 1..7.
-type ldom()     :: 28 | 29 | 30 | 31. % last day of month

-type t_now()    :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.

-type t_date()         :: {year(),month(),day()}.
-type t_time()         :: {hour(),minute(),second()}.
-type t_datetime()     :: {t_date(),t_time()}.
-type t_datetime1970() :: {{year1970(),month(),day()},t_time()}.

%%----------------------------------------------------------------------
%% Helper macros
%%----------------------------------------------------------------------
-define(EWGI_LOGGER_KEY, '_smak_logger').
-define(CTX_LOG(Ctx, L, F, A),
        case ewgi_api:find_data(?EWGI_LOGGER_KEY, Ctx) of
            Log when is_function(Log, 3) ->
                Log(L, F, A);
            _ ->
                ok % silently ignore
        end).

-type log_level() :: 'error' | 'info' | 'debug' | 'verbose'.

-record(media_type, {
          mime :: string(),
          description :: string(),
          parameters=[] :: [{string(), string()}]
         }).

-define(ROUTE_KEY, '_smak_route').
-define(ROUTE_TREE_KEY, '_smak_routes').
-define(ROUTE_PATH_KEY, '_smak_routepath').

-endif.
