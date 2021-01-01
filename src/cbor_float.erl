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

-module(cbor_float).

-export([decode_f16/1, decode_f32/1, decode_f64/1]).

-export_type([value/0]).

-type value() :: float() | positive_infinity | negative_infinity | nan.

-spec decode_f16(iodata()) -> cbor:float_value().
decode_f16(<<_S:1, 0:5, 0:10>>) ->
  0.0;
decode_f16(<<S:1, 0:5, F:10>>) ->
  math:pow(-1.0, S) * math:pow(2.0, -14) * (F/1024.0);
decode_f16(<<0:1, 31:5, 0:10>>) ->
  positive_infinity;
decode_f16(<<1:1, 31:5, 0:10>>) ->
  negative_infinity;
decode_f16(<<_S:1, 31:5, _F:10>>) ->
  nan;
decode_f16(<<S:1, E:5, F:10>>) ->
  math:pow(-1.0, S) * math:pow(2.0, E-15) * (1 + F/1024.0).

-spec decode_f32(iodata()) -> cbor:float_value().
decode_f32(<<_S:1, 0:8, 0:23>>) ->
  0.0;
decode_f32(<<0:1, 255:8, 0:23>>) ->
  positive_infinity;
decode_f32(<<1:1, 255:8, 0:23>>) ->
  negative_infinity;
decode_f32(<<_S:1, 255:8, _F:23>>) ->
  nan;
decode_f32(<<F:32/float>>) ->
  F.

-spec decode_f64(iodata()) -> cbor:float_value().
decode_f64(<<_S:1, 0:11, 0:52>>) ->
  0.0;
decode_f64(<<0:1, 2047:11, 0:52>>) ->
  positive_infinity;
decode_f64(<<1:1, 2047:11, 0:52>>) ->
  negative_infinity;
decode_f64(<<_S:1, 2047:11, _F:52>>) ->
  nan;
decode_f64(<<F:64/float>>) ->
  F.
