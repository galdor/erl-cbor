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

-module(cbor_util).

-export([unsigned_integer_bytes/1,
         encode_sequence_header/2, decode_sequence_header/2,
         binary_to_hex_string/1, hex_string_to_binary/1,
         list_to_map/1]).

-spec unsigned_integer_bytes(non_neg_integer()) -> binary().
unsigned_integer_bytes(I) ->
  unsigned_integer_bytes(I, []).

-spec unsigned_integer_bytes(non_neg_integer(), [byte()]) -> binary().
unsigned_integer_bytes(0, Acc) ->
  list_to_binary(Acc);
unsigned_integer_bytes(I, Acc) ->
  unsigned_integer_bytes(I bsr 8, [I band 16#ff | Acc]).

-spec encode_sequence_header(MajorType :: 0..7, Len) -> binary() when
    Len :: non_neg_integer().
encode_sequence_header(MajorType, Len) when Len =< 16#17 ->
  <<MajorType:3, Len:5>>;
encode_sequence_header(MajorType, Len) when Len =< 16#ff ->
  <<MajorType:3, 16#18:5, Len: 8>>;
encode_sequence_header(MajorType, Len) when Len =< 16#ffff ->
  <<MajorType:3, 16#19:5, Len: 16>>;
encode_sequence_header(MajorType, Len) when Len =< 16#ffffffff ->
  <<MajorType:3, 16#1a:5, Len: 32>>;
encode_sequence_header(MajorType, Len) when Len =< 16#ffffffffffffffff ->
  <<MajorType:3, 16#1b:5, Len: 64>>;
encode_sequence_header(_MajorType, Len) ->
  error({unencodable_sequence_length, Len}).

-spec decode_sequence_header(Tag, iodata()) ->
        {ok, Len, iodata()} | {error, Reason} when
    Tag :: byte(),
    Len :: non_neg_integer(),
    Reason :: truncated_sequence_header | invalid_sequence_header.
decode_sequence_header(Tag, Data) ->
  case {Tag band 16#1f, Data} of
    {Len, Rest} when Len =< 16#17 ->
      {ok, Len, Rest};
    {16#18, Data2} ->
      case Data2 of
        <<Len:8, Rest/binary>> ->
          {ok, Len, Rest};
        _ ->
          {error, truncated_sequence_header}
      end;
    {16#19, Data2} ->
      case Data2 of
        <<Len:16, Rest/binary>> ->
          {ok, Len, Rest};
        _ ->
          {error, truncated_sequence_header}
      end;
    {16#1a, Data2} ->
      case Data2 of
        <<Len:32, Rest/binary>> ->
          {ok, Len, Rest};
        _ ->
          {error, truncated_sequence_header}
      end;
    {16#1b, Data2} ->
      case Data2 of
        <<Len:64, Rest/binary>> ->
          {ok, Len, Rest};
        _ ->
          {error, truncated_sequence_header}
      end;
    {_MinorType, _Rest} ->
      {error, invalid_sequence_header}
  end.

-spec binary_to_hex_string(binary()) -> binary().
binary_to_hex_string(Bin) ->
  HexData = [io_lib:format("~2.16.0B", [Byte]) || <<Byte:8>> <= Bin],
  string:lowercase(iolist_to_binary(HexData)).

-spec hex_string_to_binary(binary()) -> binary().
hex_string_to_binary(Str) ->
  hex_string_to_binary(Str, <<>>).

-spec hex_string_to_binary(binary(), binary()) -> binary().
hex_string_to_binary(<<>>, Acc) ->
  Acc;
hex_string_to_binary(<<Digit1:8, Digit2:8, Rest/binary>>, Acc) ->
  Q1 = hex_digit_to_integer(Digit1),
  Q2 = hex_digit_to_integer(Digit2),
  Byte = (Q1 bsl 4) bor Q2,
  hex_string_to_binary(Rest, <<Acc/binary, Byte>>).

-spec hex_digit_to_integer(char()) -> 0..15.
hex_digit_to_integer(C) when C >= $0, C =< $9 ->
  C - $0;
hex_digit_to_integer(C) when C >= $a, C =< $f ->
  10 + C - $a;
hex_digit_to_integer(C) when C >= $F, C =< $F ->
  10 + C - $A.

-spec list_to_map(list()) -> map().
list_to_map(Values) ->
  list_to_map(Values, #{}).

list_to_map([], Acc) ->
  Acc;
list_to_map([Key, Value | Rest], Acc) ->
  list_to_map(Rest, Acc#{Key => Value}).
