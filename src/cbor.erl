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

-module(cbor).

-export([encode/1, encode_hex/1,
         decode/1, decode/2, decode_hex/1, decode_hex/2]).

-export_type([tag/0, value/0, simple_value/0, type/0]).

-type tag() :: non_neg_integer().

-type type() :: unsigned_integer | neg_integer | byte_string
              | utf8_string | array | map | simple | float | tag().

-type value() :: {type(), term()}.

-type simple_value() :: {simple_value, 0..255}
                      | false | true | null | undefined.

-spec encode(term()) -> iodata().
encode(Data) ->
  cbor_encoding:encode(Data).

-spec encode_hex(term()) -> binary().
encode_hex(Value) ->
  Data = iolist_to_binary(cbor_encoding:encode(Value)),
  cbor_util:binary_to_hex_string(Data).

-spec decode(iodata()) -> cbor_decoding:decoding_result(term()).
decode(Data) ->
  decode(Data, cbor_decoding:default_options()).

-spec decode(iodata(), cbor_decoding:options()) ->
        cbor_decoding:decoding_result(term()).
decode(Data, Opts) ->
  Decoder = cbor_decoding:decoder(Opts),
  cbor_decoding:decode(Decoder, Data).

-spec decode_hex(binary()) -> cbor_decoding:decoding_result(term()).
decode_hex(Value) ->
  decode_hex(Value, cbor_decoding:default_options()).

-spec decode_hex(binary(), cbor_decoding:options()) ->
        cbor_decoding:decoding_result(term()).
decode_hex(Str, Opts) ->
  Decoder = cbor_decoding:decoder(Opts),
  Bin = cbor_util:hex_string_to_binary(Str),
  case cbor_decoding:decode(Decoder, Bin) of
    {ok, Value, Rest} ->
      {ok, Value, cbor_util:binary_to_hex_string(Rest)};
    {error, Reason} ->
      {error, Reason}
  end.
