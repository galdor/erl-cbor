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

-export([default_tagged_value_interpreters/0,
         default_decoding_options/0,
         encode/1, encode_hex/1,
         decode/1, decode/2, decode_hex/1, decode_hex/2]).

-export_type([tag/0,
              tagged_value/0, simple_value/0,
              tagged_value_interpreter/0,
              decoding_options/0,
              interpretation_result/1]).

-type tag() :: non_neg_integer().

-type tagged_value() :: {tag(), term()}.

-type simple_value() :: {simple_value, 0..255}
                      | false | true | null | undefined.

-type decoding_options() :: #{max_depth => non_neg_integer(),
                              tagged_value_interpreters =>
                                #{tag() := tagged_value_interpreter()}}.

-type tagged_value_interpreter() ::
        fun((tagged_value(), decoding_options(), Depth :: non_neg_integer()) ->
               interpretation_result(term())).

-type decoding_result(ValueType) :: {ok, ValueType, iodata()} | {error, term()}.

-type interpretation_result(ValueType) :: {ok, ValueType} | {error, term()}.

-spec default_tagged_value_interpreters() ->
        #{tag() := tagged_value_interpreter()}.
default_tagged_value_interpreters() ->
  #{
    0 => fun interpret_utf8_string/3,
    1 => fun interpret_epoch_based_datetime/3,
    2 => fun interpret_positive_bignum/3,
    3 => fun interpret_negative_bignum/3,
    24 => fun interpret_cbor_value/3,
    32 => fun interpret_utf8_string/3,
    33 => fun interpret_base64url_data/3,
    34 => fun interpret_base64_data/3,
    35 => fun interpret_utf8_string/3,
    36 => fun interpret_utf8_string/3,
    55799 => fun interpret_self_described_cbor_value/3
}.

-spec default_decoding_options() -> decoding_options().
default_decoding_options() ->
  #{max_depth => 1024,
    tagged_value_interpreters => default_tagged_value_interpreters()}.

-spec encode(term()) -> iodata().
encode(Data) ->
  cbor_encoding:encode(Data).

-spec encode_hex(term()) -> binary().
encode_hex(Value) ->
  Data = iolist_to_binary(cbor_encoding:encode(Value)),
  cbor_util:binary_to_hex_string(Data).

-spec decode(iodata()) -> decoding_result(term()).
decode(Data) ->
  decode(Data, default_decoding_options()).

-spec decode(iodata(), decoding_options()) -> decoding_result(term()).
decode(Data, Opts) ->
  decode(Data, Opts, 0).

-spec decode(iodata(), decoding_options(), Depth) ->
        decoding_result(term()) when
    Depth :: non_neg_integer().
decode(_Data, #{max_depth := MaxDepth}, Depth) when Depth > MaxDepth ->
  {error, max_depth_reached};
decode(<<T:8, Data/binary>>, _Opts, _Depth) when T =< 16#17 ->
  {ok, T, Data};
decode(<<T:8, Data/binary>>, _Opts, _Depth) when T >= 16#18, T =< 16#1b ->
  decode_unsigned_integer(T, Data);
decode(<<T:8, Data/binary>>, _Opts, _Depth) when T >= 16#20, T =< 16#37 ->
  {ok, -1 - (T - 16#20), Data};
decode(<<T:8, Data/binary>>, _Opts, _Depth) when T >= 16#38, T =< 16#3b ->
  decode_negative_integer(T, Data);
decode(<<T:8, Data/binary>>, _Opts, _Depth) when T >= 16#40, T =< 16#5b ->
  decode_byte_string(T, Data);
decode(<<16#5f:8, Data/binary>>, _Opts, _Depth) ->
  decode_indefinite_length_byte_string(Data);
decode(<<T:8, Data/binary>>, _Opts, _Depth) when T >= 16#60, T =< 16#7b ->
  decode_utf8_string(T, Data);
decode(<<16#7f:8, Data/binary>>, _Opts, _Depth) ->
  decode_indefinite_length_utf8_string(Data);
decode(<<T:8, Data/binary>>, Opts, Depth) when T >= 16#80, T =< 16#9b ->
  decode_array(T, Data, Opts, Depth);
decode(<<16#9f:8, Data/binary>>, Opts, Depth) ->
  decode_indefinite_length_array(Data, Opts, Depth);
decode(<<T:8, Data/binary>>, Opts, Depth) when T >= 16#a0, T =< 16#bb ->
  decode_map(T, Data, Opts, Depth);
decode(<<16#bf:8, Data/binary>>, Opts, Depth) ->
  decode_indefinite_length_map(Data, Opts, Depth);
decode(<<T:8, Data/binary>>, Opts, Depth) when T >= 16#c0, T =< 16#db ->
  decode_tagged_value(T, Data, Opts, Depth);
decode(<<T:8, Data/binary>>, _Opts, _Depth) when T >= 16#e0, T =< 16#f8 ->
  decode_simple_value(T, Data);
decode(<<T, Data/binary>>, _Opts, _Depth) when T >= 16#f9, T =< 16#fb ->
  decode_float(T, Data);
decode(<<T:8, _Data/binary>>, _Opts, _Depth) ->
  {error, {invalid_type_tag, T}};
decode(<<>>, _Opts, _Depth) ->
  {error, no_input}.

-spec decode_hex(binary()) -> decoding_result(term()).
decode_hex(Value) ->
  decode_hex(Value, default_decoding_options()).

-spec decode_hex(binary(), decoding_options()) -> decoding_result(term()).
decode_hex(Str, Opts) ->
  Bin = cbor_util:hex_string_to_binary(Str),
  case decode(Bin, Opts) of
    {ok, Value, Rest} ->
      {ok, Value, cbor_util:binary_to_hex_string(Rest)};
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_unsigned_integer(Type, iodata()) ->
        decoding_result(non_neg_integer()) when
    Type :: 16#18..16#1b.
decode_unsigned_integer(16#18, <<I:8, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#19, <<I:16, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#1a, <<I:32, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(16#1b, <<I:64, Rest/binary>>) ->
  {ok, I, Rest};
decode_unsigned_integer(_Type, _Data) ->
  {error, truncated_unsigned_integer}.

-spec decode_negative_integer(Type, iodata()) ->
        decoding_result(neg_integer()) when
    Type :: 16#38..16#3b.
decode_negative_integer(16#38, <<I:8, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#39, <<I:16, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#3a, <<I:32, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(16#3b, <<I:64, Rest/binary>>) ->
  {ok, -1 - I, Rest};
decode_negative_integer(_Type, _Data) ->
  {error, truncated_negative_integer}.

-spec decode_byte_string(Type, iodata()) -> decoding_result(binary()) when
    Type :: 16#40..16#5b.
decode_byte_string(Type, Data) ->
  case cbor_util:decode_sequence_header(Type, Data) of
    {ok, Len, Data2} ->
      case Data2 of
        <<Bin:Len/binary, Rest/binary>> ->
          {ok, iolist_to_binary(Bin), Rest};
        _ ->
          {error, truncated_byte_string}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_byte_string(iodata()) ->
        decoding_result(binary()).
decode_indefinite_length_byte_string(Data) ->
  case binary:match(Data, <<255:8>>) of
    {Off, _Len} ->
      <<Bin:Off/binary, 255:8, Rest/binary>> = Data,
      {ok, Bin, Rest};
    nomatch ->
      {error, truncated_byte_string}
  end.

-spec decode_utf8_string(Type, iodata()) -> decoding_result(binary()) when
    Type :: 16#60..16#7b.
decode_utf8_string(Type, Data) ->
  case cbor_util:decode_sequence_header(Type, Data) of
    {ok, Len, Data2} ->
      case Data2 of
        <<Bin:Len/binary, Rest/binary>> ->
          case unicode:characters_to_binary(Bin) of
            Bin2 when is_binary(Bin2) ->
              {ok, Bin2, Rest};
            {error, _, _} ->
              {error, {invalid_utf8_string, Bin}};
            {incomplete, _, _} ->
              {error, {incomplete_utf8_string, Bin}}
          end;
        _ ->
          {error, truncated_utf8_string}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_utf8_string(iodata()) ->
        decoding_result(binary()).
decode_indefinite_length_utf8_string(Data) ->
  case binary:match(Data, <<255:8>>) of
    {Off, _Len} ->
      <<Bin:Off/binary, 255:8, Rest/binary>> = Data,
      case unicode:characters_to_binary(Bin) of
        Bin2 when is_binary(Bin2) ->
          {ok, Bin2, Rest};
        {error, _, _} ->
          {error, {invalid_utf8_string, Bin}};
        {incomplete, _, _} ->
          {error, {incomplete_utf8_string, Bin}}
      end;
    nomatch ->
      {error, truncated_utf8_string}
  end.

-spec decode_array(Type, iodata(), Opts, Depth) -> decoding_result(list()) when
    Type :: 16#80..16#9b,
    Opts :: decoding_options(),
    Depth :: non_neg_integer().
decode_array(Type, Data, Opts, Depth) ->
  case cbor_util:decode_sequence_header(Type, Data) of
    {ok, Len, Data2} ->
      case decode_values(Data2, Len, Opts, Depth, []) of
        {ok, Values, Rest} ->
          {ok, Values, Rest};
        {error, truncated_sequence} ->
          {error, truncated_array};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_array(iodata(), Opts, Depth) ->
        decoding_result(list()) when
    Opts :: decoding_options(),
    Depth :: non_neg_integer().
decode_indefinite_length_array(Data, Opts, Depth) ->
  case decode_indefinite_length_values(Data, Opts, Depth, []) of
    {ok, Values, Rest} ->
      {ok, Values, Rest};
    {error, truncated_sequence} ->
      {error, truncated_array};
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_map(Type, iodata(), Opts, Depth) -> decoding_result(map()) when
    Type :: 16#a0..16#bb,
    Opts :: decoding_options(),
    Depth :: non_neg_integer().
decode_map(Type, Data, Opts, Depth) ->
  case cbor_util:decode_sequence_header(Type, Data) of
    {ok, Len, Data2} ->
      case decode_values(Data2, Len*2, Opts, Depth, []) of
        {ok, Values, Rest} ->
          {ok, cbor_util:list_to_map(Values), Rest};
        {error, truncated_sequence} ->
          {error, truncated_map};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_map(iodata(), Opts, Depth) ->
        decoding_result(map()) when
    Opts :: decoding_options(),
    Depth :: non_neg_integer().
decode_indefinite_length_map(Data, Opts, Depth) ->
  case decode_indefinite_length_values(Data, Opts, Depth, []) of
    {ok, Values, _Rest} when (length(Values) rem 2) /= 0 ->
      {error, odd_number_of_map_values};
    {ok, Values, Rest} ->
      {ok, cbor_util:list_to_map(Values), Rest};
    {error, truncated_sequence} ->
      {error, truncated_map};
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_tagged_value(Type, iodata(), Opts, Depth) ->
        decoding_result(Result) when
    Type :: 16#c0..16#db,
    Opts :: decoding_options(),
    Depth :: non_neg_integer(),
    Result :: tagged_value() | term().
decode_tagged_value(Type, Data, Opts, Depth) when
    Type >= 16#c0, Type =< 16#d7 ->
  decode_tagged_data(Type - 16#c0, Data, Opts, Depth);
decode_tagged_value(16#d8, <<Tag:8, Data/binary>>, Opts, Depth) ->
  decode_tagged_data(Tag, Data, Opts, Depth);
decode_tagged_value(16#d9, <<Tag:16, Data/binary>>, Opts, Depth) ->
  decode_tagged_data(Tag, Data, Opts, Depth);
decode_tagged_value(16#da, <<Tag:32, Data/binary>>, Opts, Depth) ->
  decode_tagged_data(Tag, Data, Opts, Depth);
decode_tagged_value(16#db, <<Tag:64, Data/binary>>, Opts, Depth) ->
  decode_tagged_data(Tag, Data, Opts, Depth);
decode_tagged_value(_Type, _Data, _Opts, _Depth) ->
  {error, truncated_tagged_value}.

-spec decode_tagged_data(tag(), iodata(), decoding_options(), Depth) ->
        decoding_result(Result) when
    Depth :: non_neg_integer(),
    Result :: tagged_value() | term().
decode_tagged_data(Tag, Data, Opts, Depth) ->
  case decode(Data, Opts, Depth + 1) of
    {ok, Value, Rest} ->
      case interpret_tagged_value({Tag, Value}, Opts, Depth + 1) of
        {ok, Value2} ->
          {ok, Value2, Rest};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec interpret_tagged_value(tagged_value(), decoding_options(), Depth) ->
        interpretation_result(term()) when
    Depth :: non_neg_integer().
interpret_tagged_value(TaggedValue = {Tag, _Value},
                       Opts = #{tagged_value_interpreters := Interpreters},
                       Depth) ->
  case maps:find(Tag, Interpreters) of
    {ok, Interpreter} ->
      Interpreter(TaggedValue, Opts, Depth);
    error ->
      {ok, TaggedValue}
  end;
interpret_tagged_value(TaggedValue, _Opts, _Depth) ->
  {ok, TaggedValue}.

-spec interpret_utf8_string(tagged_value(), decoding_options(), Depth) ->
        interpretation_result(unicode:chardata()) when
    Depth :: non_neg_integer().
interpret_utf8_string({_Tag, Value}, _Opts, _Depth) when is_binary(Value) ->
  {ok, Value};
interpret_utf8_string(TaggedValue, _Opts, _Depth) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_epoch_based_datetime(tagged_value(), decoding_options(),
                                     Depth) ->
        interpretation_result(integer()) when
    Depth :: non_neg_integer().
interpret_epoch_based_datetime({_Tag, Value}, _Opts, _Depth) when
    is_integer(Value) ->
  {ok, Value * 1000000000};
interpret_epoch_based_datetime({_Tag, Value}, _Opts, _Depth) when
    is_float(Value) ->
  {ok, round(Value * 1.0e9)};
interpret_epoch_based_datetime(TaggedValue, _Opts, _Depth) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_positive_bignum(tagged_value(), decoding_options(), Depth) ->
        interpretation_result(integer()) when
    Depth :: non_neg_integer().
interpret_positive_bignum({_Tag, Value}, _Opts, _Depth) when is_binary(Value) ->
  Size = byte_size(Value) * 8,
  <<N:Size>> = Value,
  {ok, N};
interpret_positive_bignum(TaggedValue, _Opts, _Depth) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_negative_bignum(tagged_value(), decoding_options(), Depth) ->
        interpretation_result(integer()) when
    Depth :: non_neg_integer().
interpret_negative_bignum({_Tag, Value}, _Opts, _Depth) when is_binary(Value) ->
  Size = byte_size(Value) * 8,
  <<N:Size>> = Value,
  {ok, -1 - N};
interpret_negative_bignum(TaggedValue, _Opts, _Depth) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_base64url_data(tagged_value(), decoding_options(), Depth) ->
        interpretation_result(binary()) when
    Depth :: non_neg_integer().
interpret_base64url_data({_Tag, Value}, _Opts, _Depth) when is_binary(Value) ->
  case cbor_base64url:decode(Value) of
    {ok, Bin} ->
      {ok, Bin};
    {error, Reason} ->
      {error, {invalid_base64url_data, Reason}}
  end;
interpret_base64url_data(TaggedValue, _Opts, _Depth) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_base64_data(tagged_value(), decoding_options(), Depth) ->
        interpretation_result(binary()) when
    Depth :: non_neg_integer().
interpret_base64_data({_Tag, Value}, _Opts, _Depth) when is_binary(Value) ->
  case cbor_base64:decode(Value) of
    {ok, Bin} ->
      {ok, Bin};
    {error, Reason} ->
      {error, {invalid_base64_data, Reason}}
  end;
interpret_base64_data(TaggedValue, _Opts, _Depth) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_cbor_value(tagged_value(), decoding_options(), Depth) ->
        interpretation_result(term()) when
    Depth :: non_neg_integer().
interpret_cbor_value({_Tag, Value}, Opts, Depth) when is_binary(Value) ->
  case decode(Value, Opts, Depth) of
    {ok, Value2, <<>>} ->
      {ok, Value2};
    {ok, _Value2, Rest} ->
      {error, {invalid_trailing_data, Rest}};
    {error, Reason} ->
      {error, {invalid_cbor_data, Reason}}
  end;
interpret_cbor_value(TaggedValue, _Opts, _Depth) ->
  {error, {invalid_tagged_value, TaggedValue}}.

-spec interpret_self_described_cbor_value(tagged_value(), decoding_options(),
                                          Depth) ->
        interpretation_result(term()) when
    Depth :: non_neg_integer().
interpret_self_described_cbor_value({_Tag, Value}, _Opts, _Depth) ->
  {ok, Value}.

-spec decode_simple_value(Type, iodata()) ->
        decoding_result(simple_value()) when
    Type :: 16#e9..16#f8.
decode_simple_value(Type, <<Data/binary>>) when Type >= 16#e0, Type =< 16#f3 ->
  {ok, {simple_value, Type - 16#e0}, Data};
decode_simple_value(16#f4, <<Data/binary>>) ->
  {ok, false, Data};
decode_simple_value(16#f5, <<Data/binary>>) ->
  {ok, true, Data};
decode_simple_value(16#f6, <<Data/binary>>) ->
  {ok, null, Data};
decode_simple_value(16#f7, <<Data/binary>>) ->
  {ok, undefined, Data};
decode_simple_value(16#f8, <<Value:8, Data/binary>>) ->
  {ok, {simple_value, Value}, Data};
decode_simple_value(16#f8, <<>>) ->
  {error, truncated_simple_value}.

-spec decode_float(Type, iodata()) -> decoding_result(cbor_float:value()) when
    Type :: 16#f9..16#fb.
%% Half-precision
decode_float(16#f9, <<Data:2/binary, Rest/binary>>) ->
  {ok, cbor_float:decode_f16(Data), Rest};
%% Single precision
decode_float(16#fa, <<Data:4/binary, Rest/binary>>) ->
  {ok, cbor_float:decode_f32(Data), Rest};
%% Double precision
decode_float(16#fb, <<Data:8/binary, Rest/binary>>) ->
  {ok, cbor_float:decode_f64(Data), Rest};
%% Truncated
decode_float(_Type, _Data) ->
  {error, truncated_float}.

-spec decode_values(iodata(), N, Opts, Depth, list()) ->
        decoding_result(list()) when
    N :: non_neg_integer(),
    Opts :: decoding_options(),
    Depth :: non_neg_integer().
decode_values(Data, 0, _Opts, _Depth, Acc) ->
  {ok, lists:reverse(Acc), Data};
decode_values(<<>>, _N, _Opts, _Depth, _Acc) ->
  {error, truncated_sequence};
decode_values(Data, N, Opts, Depth, Acc) ->
  case decode(Data, Opts, Depth + 1) of
    {ok, Value, Rest} ->
      decode_values(Rest, N-1, Opts, Depth, [Value | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.

-spec decode_indefinite_length_values(iodata(), Opts, Depth, list()) ->
        decoding_result(list()) when
    Opts :: decoding_options(),
    Depth :: non_neg_integer().
decode_indefinite_length_values(<<>>, _Opts, _Depth, _Acc) ->
  {error, truncated_sequence};
decode_indefinite_length_values(<<16#ff:8, Data/binary>>, _Opts, _Depth, Acc) ->
  {ok, lists:reverse(Acc), Data};
decode_indefinite_length_values(Data, Opts, Depth, Acc) ->
  case decode(Data, Opts, Depth + 1) of
    {ok, Value, Rest} ->
      decode_indefinite_length_values(Rest, Opts, Depth, [Value | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.
