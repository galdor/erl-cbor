%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(cbor_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
  Encode = fun (Value) ->
               cbor:encode_hex(Value)
           end,
  %% Integers
  ?assertEqual("00", Encode(0)),
  ?assertEqual("01", Encode(1)),
  ?assertEqual("0a", Encode(10)),
  ?assertEqual("17", Encode(23)),
  ?assertEqual("1818", Encode(24)),
  ?assertEqual("1819", Encode(25)),
  ?assertEqual("1864", Encode(100)),
  ?assertEqual("1903e8", Encode(1000)),
  ?assertEqual("1a000f4240", Encode(1000000)),
  ?assertEqual("1b000000e8d4a51000", Encode(1000000000000)),
  ?assertEqual("1bffffffffffffffff", Encode(18446744073709551615)),
  ?assertEqual("c249010000000000000000", Encode(18446744073709551616)),
  ?assertEqual("3bffffffffffffffff", Encode(-18446744073709551616)),
  ?assertEqual("c349010000000000000000", Encode(-18446744073709551617)),
  ?assertEqual("20", Encode(-1)),
  ?assertEqual("29", Encode(-10)),
  ?assertEqual("3863", Encode(-100)),
  ?assertEqual("3903e7", Encode(-1000)),
  %% Floating point numbers
  ?assertEqual("fb0000000000000000", Encode(0.0)), % canonical: f90000
  ?assertEqual("fb0000000000000000", Encode(-0.0)), % canonical: f98000
  ?assertEqual("fb3ff0000000000000", Encode(1.0)), % canonical: f93c00
  ?assertEqual("fb3ff199999999999a", Encode(1.1)),
  ?assertEqual("fb3ff8000000000000", Encode(1.5)), % canonical: f93e00
  ?assertEqual("fb40effc0000000000", Encode(65504.0)), % canonical: f97bff
  ?assertEqual("fb40f86a0000000000", Encode(100000.0)), % canonical: fa47c35000
  ?assertEqual("fb47efffffe0000000", Encode(3.4028234663852886e+38)), % canonical: fa7f7fffff
  ?assertEqual("fb7e37e43c8800759c", Encode(1.0e+300)),
  ?assertEqual("fb3e70000000000000", Encode(5.960464477539063e-8)), % canonical: f90001
  ?assertEqual("fb3f10000000000000", Encode(0.00006103515625)), % canonical: f90400
  ?assertEqual("fbc010000000000000", Encode(-4.0)), % canonical: f9c400
  ?assertEqual("fbc010666666666666", Encode(-4.1)),
  ?assertEqual("f97c00", Encode(infinity)),
  ?assertEqual("f97c00", Encode(positive_infinity)),
  ?assertEqual("f9fc00", Encode(negative_infinity)),
  ?assertEqual("f97e00", Encode(nan)),
  %% Booleans
  ?assertEqual("f4", Encode(false)),
  ?assertEqual("f5", Encode(true)),
  %% Strings
  ?assertEqual("60", Encode({string, <<"">>})),
  ?assertEqual("6161", Encode({string, <<"a">>})),
  ?assertEqual("6449455446", Encode({string, <<"IETF">>})),
  ?assertEqual("6449455446", Encode({string, "IETF"})),
  ?assertEqual("62225c", Encode({string, <<"\"\\">>})),
  ?assertEqual("62c3bc", Encode({string, <<16#fc/utf8>>})),
  ?assertEqual("63e6b0b4", Encode({string, <<16#6c34/utf8>>})),
  ?assertEqual("64f0908591", Encode({string, <<16#10151/utf8>>})),
  %% Binary data
  ?assertEqual("40", Encode(<<>>)),
  ?assertEqual("43010203", Encode(<<1, 2, 3>>)),
  ?assertEqual("58180102030405060708090a0b0c0d0e0f101112131415161718",
               Encode(<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                        17, 18, 19, 20, 21, 22, 23, 24>>)),
  %% Lists
  ?assertEqual("80", Encode([])),
  ?assertEqual("83010203", Encode([1, 2, 3])),
  ?assertEqual("8301820203820405", Encode([1, [2, 3], [4, 5]])),
  ?assertEqual("98190102030405060708090a0b0c0d0e0f101112131415161718181819",
               Encode([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                       17, 18, 19, 20, 21, 22, 23, 24, 25])),
  %% Maps
  ?assertEqual("a0", Encode(#{})),
  ?assertEqual("a201020304", Encode(#{1 => 2, 3 => 4})),
  %% Mixed lists and maps
  ?assertEqual("a24161014162820203",
               Encode(#{<<"a">> => 1, <<"b">> => [2, 3]})),
  ?assertEqual("824161a141624163",
               Encode([<<"a"/utf8>>, #{<<"b">> => <<"c">>}])),
  %% Simple values
  ?assertEqual("f6", Encode(null)),
  ?assertEqual("f7", Encode(undefined)),
  %% Tagged values
  ?assertEqual("c074323031332d30332d32315432303a30343a30305a",
               Encode({0, {string, <<"2013-03-21T20:04:00Z">>}})),
  ?assertEqual("c11a514b67b0", Encode({1, 1363896240})),
  ?assertEqual("c1fb41d452d9ec200000", Encode({1, 1363896240.5})),
  ?assertEqual("d74401020304", Encode({23, <<1, 2, 3, 4>>})),
  ?assertEqual("d818456449455446",
               Encode({24, <<16#64, 16#49, 16#45, 16#54, 16#46>>})),
  ?assertEqual("d82076687474703a2f2f7777772e6578616d706c652e636f6d",
               Encode({32, {string, <<"http://www.example.com">>}})),
  %% Dates
  ?assertEqual("c074313937302d30312d30315430303a30303a30305a",
               Encode({datetime, 0})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, 1587856038})),
  ?assertEqual("c07819323032302d30342d32365430303a30373a31382b30313a3030",
               Encode({datetime, 1587856038, 3600})),
  ?assertEqual("c07819323032302d30342d32365430303a30373a31382b30313a3030",
               Encode({datetime, 1587856038, {1}})),
  ?assertEqual("c07819323032302d30342d32355431393a33373a31382d30333a3330",
               Encode({datetime, 1587856038, {-3, -30}})).
