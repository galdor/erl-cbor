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

-module(cbor_base64).

-export([decode/1]).

-spec decode(iodata()) -> {ok, binary()} | {error, term()}.
decode(Data) ->
  try decode(Data, <<>>) of
    Result ->
      Result
  catch
    error:Reason ->
      {error, Reason}
  end.

decode(<<>>, Acc) ->
  {ok, Acc};
decode(<<D1:8, D2:8, $=:8, $=:8>>, Acc) ->
  V1 = digit_value(D1),
  V2 = digit_value(D2) bsr 4,
  Data = <<V1:6, V2:2>>,
  decode(<<>>, <<Acc/binary, Data/binary>>);
decode(<<D1:8, D2:8>>, Acc) ->
  V1 = digit_value(D1),
  V2 = digit_value(D2) bsr 4,
  Data = <<V1:6, V2:2>>,
  decode(<<>>, <<Acc/binary, Data/binary>>);
decode(<<D1:8, D2:8, D3:8, $=:8>>, Acc) ->
  V1 = digit_value(D1),
  V2 = digit_value(D2),
  V3 = digit_value(D3) bsr 2,
  Data = <<V1:6, V2:6, V3:4>>,
  decode(<<>>, <<Acc/binary, Data/binary>>);
decode(<<D1:8, D2:8, D3:8>>, Acc) ->
  V1 = digit_value(D1),
  V2 = digit_value(D2),
  V3 = digit_value(D3) bsr 2,
  Data = <<V1:6, V2:6, V3:4>>,
  decode(<<>>, <<Acc/binary, Data/binary>>);
decode(<<D1:8, D2:8, D3:8, D4:8, Rest/binary>>, Acc) ->
  V1 = digit_value(D1),
  V2 = digit_value(D2),
  V3 = digit_value(D3),
  V4 = digit_value(D4),
  Data = <<V1:6, V2:6, V3:6, V4:6>>,
  decode(Rest, <<Acc/binary, Data/binary>>);
decode(Data, _Acc) ->
  {error, {invalid_data, Data}}.

-spec digit_value($A..$Z | $a..$z | $0..$9 | $+ | $/) -> 0..63.
digit_value(Digit) when Digit >= $A, Digit =< $Z ->
  Digit - $A;
digit_value(Digit) when Digit >= $a, Digit =< $z ->
  26 + Digit - $a;
digit_value(Digit) when Digit >= $0, Digit =< $9 ->
  52 + Digit - $0;
digit_value($+) ->
  62;
digit_value($/) ->
  63;
digit_value(C) ->
  error({invalid_base64_digit, C}).
