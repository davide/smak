%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Implementation of compressed, signed, encrypted share-nothing
%% cookie-based sessions.
%%
%% The binary data is first compressed, then encrypted using AES in CFB mode
%% with a random initialization vector. The data is timestamped and signed
%% using HMAC-SHA1 with the same key.
%%
%% When the cookie is decode, the signature is first checked for
%% validity. Then, the timestamp is checked for expiration. Finally, the data
%% is decrypted, decompressed, and deserialized.
%%
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(smak_sn_cookie).
-author('Hunter Morris <hunter.morris@smarkets.com>').

-define(MAX_SIZE, 4096). %% Most browsers only allow 4K of cookies
-define(MAX_SIZE_REPR, 2048). %% Assume a 2-character representation of each byte in the final cookie.

-export([encode/2, encode/3, decode/3]).

%% @spec encode(Key::binary(), Data::binary()) -> binary() | {error, Reason::atom()}
%% @doc Encodes and signs the data using the specified secret key.
%% @see encode/3
-spec encode(binary(), binary()) -> binary() | {error, atom()}.
encode(Key, Data) ->
    encode(Key, Data, ?MAX_SIZE_REPR).

%% @spec encode(Key::binary(), Data::binary(), MaxSize::integer()) -> binary() | {error, Reason::atom()}
%% @doc Encodes and signs the data using the specified secret key and provides an error if it exceeds the maximum size.
-spec encode(binary(), binary(), integer()) -> binary() | {error, atom()}.
encode(<<Key:16/binary>>, Data, MaxSize) when MaxSize > 0, is_binary(Data) ->
    IV = crypto:rand_bytes(16),
    DataCompressed = zlib:zip(Data),
    DataCrypted = crypto:aes_cfb_128_encrypt(Key, IV, DataCompressed),
    Timestamp = smak_calendar:now_utc_ts_ms(),
    HMACSignature = crypto:sha_mac(Key, <<Timestamp:48/integer, IV:16/binary, DataCrypted/bits>>),
    Result = <<HMACSignature:20/binary, Timestamp:48/integer, IV:16/binary, DataCrypted/bits>>,
    case size(Result) of
        Sz when Sz >= MaxSize ->
            {error, session_too_large};
        _ ->
            Result
    end.
    
%% @spec decode(Key::binary(), Cookie::binary(), Timeout::integer()) -> binary() | {error, Reason::atom()}
%% @doc Checks the signature and timeout and decrypts the cookie if it is valid.
-spec decode(binary(), binary(), integer()) -> binary() | {error, atom()}.
decode(<<Key:16/binary>>, <<HMACSignature:20/binary, Timestamp:48/integer, IV:16/binary, DataCrypted/bits>>, Timeout) ->
    MinimumAge = smak_calendar:now_utc_ts_ms() - Timeout,
    case crypto:sha_mac(Key, <<Timestamp:48/integer, IV:16/binary, DataCrypted/bits>>) of
        HMACSignature ->
            case Timestamp - MinimumAge of
                Diff when Diff =< 0 ->
                    {error, session_timeout};
                _ ->
                    DataCompressed = crypto:aes_cfb_128_decrypt(Key, IV, DataCrypted),
                    Data = zlib:unzip(DataCompressed),
                    Data
            end;
        _ ->
            {error, cookie_tampered}
    end;
decode(<<_:16/binary>>, _, _) ->
    {error, invalid_cookie}.
