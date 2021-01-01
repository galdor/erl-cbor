%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cbor_base64_test).

-include_lib("eunit/include/eunit.hrl").

decode_test() ->
  Decode = fun (BinStr) ->
               {ok, Bin} = cbor_base64:decode(BinStr),
               Bin
           end,
  ?assertEqual(<<"abc">>, Decode(<<"YWJj">>)),
  ?assertEqual(<<"abcDEF123+//">>, Decode(<<"YWJjREVGMTIzKy8v">>)),
  ?assertEqual(<<"abcd">>, Decode(<<"YWJjZA==">>)),
  ?assertEqual(<<"abcd">>, Decode(<<"YWJjZA">>)),
  ?assertEqual(<<"abcde">>, Decode(<<"YWJjZGU=">>)),
  ?assertEqual(<<"abcde">>, Decode(<<"YWJjZGU">>)),
  ?assertEqual(<<"ab">>, Decode(<<"YWI=">>)),
  ?assertEqual(<<"a">>, Decode(<<"YQ==">>)),
  ?assertEqual(<<"a">>, Decode(<<"YQ">>)),
  ?assertEqual(<<"">>, Decode(<<"">>)),
  ?assertEqual(<<16#fb>>, Decode(<<"+/==">>)),
  ?assertEqual(<<16#fb>>, Decode(<<"+/">>)).
