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
  %% Floats
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
  ?assertEqual("824161a141624163", Encode([<<"a">>, #{<<"b">> => <<"c">>}])),
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
  ?assertEqual("c074313936392d31322d32305431303a31333a32305a",
               Encode({datetime, -1000000})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, 1587856038})),
  ?assertEqual("c07819323032302d30342d32365430303a30373a31382b30313a3030",
               Encode({datetime, 1587856038, 3600})),
  ?assertEqual("c07819323032302d30342d32355431393a33373a31382d30333a3330",
               Encode({datetime, 1587856038, -12600})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, {1587856038, second}})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, {1587856038100, millisecond}})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, {1587856038200100, microsecond}})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, {1587856038300200100, nanosecond}})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, {{2020, 4, 25}, {23, 07, 18}}})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, {{2020, 4, 25}, {23, 07, 18}}, 0})),
  ?assertEqual("c07819323032302d30342d32365430303a30373a31382b30313a3030",
               Encode({datetime, {{2020, 4, 25}, {23, 07, 18}}, 3600})),
  ?assertEqual("c07819323032302d30342d32355431393a33373a31382d30333a3330",
               Encode({datetime, {{2020, 4, 25}, {23, 07, 18}}, -12600})),
  ?assertEqual("c074323032302d30342d32355432333a30373a31385a",
               Encode({datetime, {1587, 856038, 0}})),
  %% Timestamps
  ?assertEqual("c100",
               Encode({timestamp, 0})),
  ?assertEqual("c13a000f423f",
               Encode({timestamp, -1000000})),
  ?assertEqual("c11a5ea4c2a6",
               Encode({timestamp, 1587856038})),
  ?assertEqual("c11a5ea4c2a6",
               Encode({timestamp, {1587856038, second}})),
  ?assertEqual("c1fb41d7a930a9866666",
               Encode({timestamp, {1587856038100, millisecond}})),
  ?assertEqual("c1fb41d7a930a98cce70",
               Encode({timestamp, {1587856038200100, microsecond}})),
  ?assertEqual("c1fb41d7a930a993367a",
               Encode({timestamp, {1587856038300200100, nanosecond}})),
  ?assertEqual("c11a5ea4c2a6",
               Encode({timestamp, {{2020, 4, 25}, {23, 07, 18}}})),
  ?assertEqual("c1fb41d7a930a9866738",
               Encode({timestamp, {1587, 856038, 100050}})).

decode_test() ->
  Decode = fun (String) ->
               {ok, Value, _Rest} = cbor:decode_hex(String),
               Value
           end,
  %% Integers
  ?assertEqual(0, Decode("00")),
  ?assertEqual(1, Decode("01")),
  ?assertEqual(10, Decode("0a")),
  ?assertEqual(23, Decode("17")),
  ?assertEqual(24, Decode("1818")),
  ?assertEqual(25, Decode("1819")),
  ?assertEqual(100, Decode("1864")),
  ?assertEqual(1000, Decode("1903e8")),
  ?assertEqual(1000000, Decode("1a000f4240")),
  ?assertEqual(1000000000000, Decode("1b000000e8d4a51000")),
  ?assertEqual(18446744073709551615, Decode("1bffffffffffffffff")),
  ?assertEqual(-18446744073709551616, Decode("3bffffffffffffffff")),
  ?assertEqual(-1, Decode("20")),
  ?assertEqual(-10, Decode("29")),
  ?assertEqual(-100, Decode("3863")),
  ?assertEqual(-1000, Decode("3903e7")),
  %% Byte strings
  ?assertEqual(<<>>, Decode("40")),
  ?assertEqual(<<1, 2, 3>>, Decode("43010203")),
  ?assertEqual(<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                 17, 18, 19, 20, 21, 22, 23, 24>>,
               Decode("58180102030405060708090a0b0c0d0e0f101112131415161718")),
  %% Indefinite length byte strings
  ?assertEqual(<<>>, Decode("5fff")),
  ?assertEqual(<<1, 2, 3>>, Decode("5f010203ff")),
  %% UTF-8 strings
  ?assertEqual(<<"">>, Decode("60")),
  ?assertEqual(<<"a">>, Decode("6161")),
  ?assertEqual(<<"IETF">>, Decode("6449455446")),
  ?assertEqual(<<"\"\\">>, Decode("62225c")),
  ?assertEqual(<<16#fc/utf8>>, Decode("62c3bc")),
  ?assertEqual(<<16#6c34/utf8>>, Decode("63e6b0b4")),
  ?assertEqual(<<16#10151/utf8>>, Decode("64f0908591")),
  %% Indefinite length UTF-8 strings
  ?assertEqual(<<"">>, Decode("7fff")),
  ?assertEqual(<<"a">>, Decode("7f61ff")),
  ?assertEqual(<<"IETF">>, Decode("7f49455446ff")),
  %% Arrays
  ?assertEqual([], Decode("80")),
  ?assertEqual([1, 2, 3], Decode("83010203")),
  ?assertEqual([1, [2, 3], [4, 5]], Decode("8301820203820405")),
  ?assertEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                17, 18, 19, 20, 21, 22, 23, 24, 25],
               Decode("98190102030405060708090a0b0c0d0e0f101112131415161718181819")),
  %% Indefinite-length arrays
  ?assertEqual([], Decode("9fff")),
  ?assertEqual([1, 2, 3], Decode("9f010203ff")),
  ?assertEqual([1, [2, 3], [4, 5]], Decode("9f018202039f0405ffff")),
  ?assertEqual([1, [2, 3], [4, 5]], Decode("83018202039f0405ff")),
  ?assertEqual([1, [2, 3], [4, 5]], Decode("83018202039f0405ff")),
  ?assertEqual([1, [2, 3], [4, 5]], Decode("83019f0203ff820405")),
  %% Maps
  ?assertEqual(#{}, Decode("a0")),
  ?assertEqual(#{1 => 2, 3 => 4}, Decode("a201020304ff")),
  %% Indefinite-length maps
  ?assertEqual(#{}, Decode("bfff")),
  ?assertEqual(#{1 => 2, 3 => 4}, Decode("bf01020304ff")),
  ?assertEqual(#{<<"Fun">> => true, <<"Amt">> => -2},
               Decode("bf6346756ef563416d7421ff")),
  %% Mixed arrays and maps
  ?assertEqual(#{<<"a">> => 1, <<"b">> => [2, 3]},
               Decode("a24161014162820203")),
  ?assertEqual(#{<<"a">> => 1, <<"b">> => [2, 3]},
               Decode("bf61610161629f0203ffff")),
  ?assertEqual([<<"a">>, #{<<"b">> => <<"c">>}], Decode("824161a141624163")),
  %% Tagged values
  ?assertEqual({6, 0}, Decode("c600")),
  ?assertEqual({42, true}, Decode("d82af5")),
  ?assertEqual({4114, 1}, Decode("d9101201")),
  ?assertEqual({251658240, null}, Decode("da0f000000f6")),
  ?assertEqual({1311768465173141112, 0}, Decode("db123456781234567800")),
  ?assertEqual(<<"http://example.com">>, % URI
               Decode("d82072687474703a2f2f6578616d706c652e636f6d")),
  %% Tagged values - bignums
  ?assertEqual(18446744073709551616, Decode("c249010000000000000000")),
  ?assertEqual(-18446744073709551617, Decode("c349010000000000000000")),
  %% Tagged values - base64url-encoded data
  ?assertEqual(<<"">>, Decode("d82160")),
  ?assertEqual(<<"hello">>, Decode("d8216761475673624738")), % "aGVsbG8K"
  ?assertEqual(<<16#fb>>, Decode("d821622d5f")), % "-_"
  %% Tagged values - base64-encoded data
  ?assertEqual(<<"">>, Decode("d82260")),
  ?assertEqual(<<"hello">>, Decode("d8226761475673624738")), % "aGVsbG8K"
  ?assertEqual(<<16#fb>>, Decode("d822622b2f")), % "+/"
  %% Tagged values - CBOR-encoded value
  ?assertEqual(<<"abc">>, Decode("d8184443616263")),
  %% Tagged values - regular expression
  ?assertEqual(<<"a+b?">>, Decode("d82364612b623f")),
  %% Tagged values - MIME message
  ?assertEqual(<<"Subject: hello\r\n\r\nworld">>, % MIME message
               Decode("d824775375626a6563743a2068656c6c6f0d0a0d0a776f726c64")),
  %% Tagged values - self-described CBOR value
  ?assertEqual(42, Decode("d9d9f7182a")),
  %% Simple values
  ?assertEqual({simple_value, 0}, Decode("e0")),
  ?assertEqual({simple_value, 10}, Decode("ea")),
  ?assertEqual({simple_value, 19}, Decode("f3")),
  ?assertEqual(false, Decode("f4")),
  ?assertEqual(true, Decode("f5")),
  ?assertEqual(null, Decode("f6")),
  ?assertEqual(undefined, Decode("f7")),
  ?assertEqual({simple_value, 0}, Decode("f800")),
  ?assertEqual({simple_value, 255}, Decode("f8ff")),
  %% Floating point numbers
  ?assertEqual(0.0, Decode("f90000")),
  ?assertEqual(-0.0, Decode("f98000")),
  ?assertEqual(1.0, Decode("f93c00")),
  ?assertEqual(1.1, Decode("fb3ff199999999999a")),
  ?assertEqual(1.5, Decode("f93e00")),
  ?assertEqual(65504.0, Decode("f97bff")),
  ?assertEqual(100000.0, Decode("fa47c35000")),
  ?assertEqual(3.4028234663852886e+38, Decode("fa7f7fffff")),
  ?assertEqual(1.0e+300, Decode("fb7e37e43c8800759c")),
  ?assertEqual(5.960464477539063e-8, Decode("f90001")),
  ?assertEqual(0.00006103515625, Decode("f90400")),
  ?assertEqual(-4.0, Decode("f9c400")),
  ?assertEqual(-4.1, Decode("fbc010666666666666")),
  ?assertEqual(positive_infinity, Decode("f97c00")),
  ?assertEqual(positive_infinity, Decode("fa7f800000")),
  ?assertEqual(positive_infinity, Decode("fb7ff0000000000000")),
  ?assertEqual(negative_infinity, Decode("f9fc00")),
  ?assertEqual(negative_infinity, Decode("faff800000")),
  ?assertEqual(negative_infinity, Decode("fbfff0000000000000")),
  ?assertEqual(nan, Decode("f97e00")),
  ?assertEqual(nan, Decode("fa7fc00000")),
  ?assertEqual(nan, Decode("fb7ff8000000000000")).
