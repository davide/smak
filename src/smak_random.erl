%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Smak utilities for generating random numbers.
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php
%%
%% Some code is based on the Python Paste Project which is copyright Ian
%% Bicking, Clark C. Evans, and contributors and released under the MIT
%% license. See: http://pythonpaste.org/

-module(smak_random).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-export([init/0, uniform/0, uniform/1, bytes/1]).

-spec(init/0 :: () -> ok | {error, any()}).

init() ->
    case crypto:start() of
        ok -> ok;
        {error,{already_started,crypto}} -> ok;
        Err -> Err
    end.

-spec(uniform/0 :: () -> float()).

uniform() ->
    %% This is straight from AS 183, so it might not be good enough for
    %% cryptography even though A1-3 come from a different generator.
    A1 = uniform(30000),
    A2 = uniform(30000),
    A3 = uniform(30000),
    R = A1/30269 + A2/30307 + A3/30323,
    R - trunc(R).

-spec(uniform/1 :: (integer()) -> integer()).

%% @doc N => 1, gives an integer between 1 and N             
uniform(N) ->
    crypto:rand_uniform(1, N).

-spec(bytes/1 :: (integer()) -> binary()).

bytes(N) ->
    crypto:rand_bytes(N).
