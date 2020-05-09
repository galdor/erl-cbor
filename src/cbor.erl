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

%% @doc The cbor module is an implementation of the CBOR data encoding format
%% as defined by RFC 7049.
%%
%% @reference See <a href="https://tools.ietf.org/html/rfc7049">RFC 7049</a>.
-module(cbor).

-export([default_tagged_value_interpreters/0,
         default_decoding_options/0,
         encode/1, encode_hex/1,
         decode/1, decode/2, decode_hex/1, decode_hex/2]).

-export_type([tag/0,
              tagged_value/0, simple_value/0, float_value/0,
              tagged_value_interpreter/0,
              decoding_options/0,
              interpretation_result/1]).

-type tag() :: non_neg_integer().
%% A semantic tag as defined in RFC 7049 2.4.

-type tagged_value() :: {tag(), term()}.
%% A CBOR tagged value, i.e. a value with an integer tag acting like a
%% semantic hint.

-type simple_value() :: {simple_value, 0..255}
                      | false | true | null | undefined.
%% A simple CBOR value as defined in RFC 7049 2.3.

-type float_value() :: float() | positive_infinity | negative_infinity | nan.
%% A floating point type extending the standard `float/0' type to support
%% IEEE.754 special values. Note that we do not include a value for negative
%% zero since Erlang accepts the `-0' syntax but considers it equal to `0`; we
%% could add a `negative_zero' value, but it would be inconsistent compared to
%% the handling of positive zero.

-type decoding_options() :: #{tagged_value_interpreters =>
                                #{tag() := tagged_value_interpreter()}}.
%% A set of options affecting CBOR decoding:
%% <dl>
%%   <dt>`tagged_value_interpreters'</dt>
%%   <dd>
%%     A map containing a tagged value interpreter function for each supported
%%     tagged value. Unsupported tagged values will be decoded to a tuple of
%%     the form `{Tag, Value}'.
%%   </dd>
%% </dl>

-type tagged_value_interpreter() :: fun((tagged_value()) ->
                                           interpretation_result(term())).
%% A function used to interpret a CBOR tagged value.

-type decoding_result(ValueType) :: {ok, ValueType, iodata()} | {error, term()}.
%% The type of values returned by decoding functions.

-type interpretation_result(ValueType) :: {ok, ValueType} | {error, term()}.
%% The type of values returned by interpretation functions.

%% @doc Return the default map of tagged value interpreters.
-spec default_tagged_value_interpreters() ->
        #{tag() := tagged_value_interpreter()}.
default_tagged_value_interpreters() ->
  #{
    %% 0 => fun interpret_standard_datetime/1,
    %% 1 => fun interpret_epoch_based_datetime/1,
    2 => fun interpret_positive_bignum/1,
    3 => fun interpret_negative_bignum/1,
    24 => fun interpret_cbor_value/1,
    32 => fun interpret_value/1,
    33 => fun interpret_base64url_data/1,
    34 => fun interpret_base64_data/1,
    35 => fun interpret_value/1,
    36 => fun interpret_value/1,
    55799 => fun interpret_value/1
}.

%% @doc Return the default list of decoding options.
-spec default_decoding_options() -> decoding_options().
default_decoding_options() ->
  #{tagged_value_interpreters => default_tagged_value_interpreters()}.

%% @doc Encode an Erlang value and return the binary representation of the
%% resulting CBOR data item.
%%
%% Integers, floats, boolean, binaries, lists and maps are encoded to the
%% associated CBOR type.
%%
%% Atoms are used for specific constants:
%% <dl>
%%   <dt>`positive_infinity'</dt>
%%   <dd>IEEE.754 positive infinity.</dd>
%%
%%   <dt>`negative_infinity'</dt>
%%   <dd>IEEE.754 negative infinity.</dd>
%%
%%   <dt>`nan'</dt>
%%   <dd>IEEE.754 NaN.</dd>
%%
%%   <dt>`null'</dt>
%%   <dd>CBOR null value.</dd>
%%
%%   <dt>`undefined'</dt>
%%   <dd>CBOR undefined value.</dd>
%% </dl>
%%
%% Tuples are used for more complex CBOR values:
%% <dl>
%%   <dt>`{string, Value}'</dt>
%%   <dd>
%%     `Value' is encoded to a CBOR string; `Value' must be of type
%%     `unicode:chardata/0'.
%%   </dd>
%%   <dt>`{datetime, Value}'</dt>
%%   <dt>`{datetime, Value, UTCOffset}'</dt>
%%   <dd>
%%     `Value' is encoded to a CBOR text string tagged as a standard datetime
%%     string. `Value' must be of type `cbor_time:datetime/0'. If present,
%%     `UTCOffset' is used to transform the universal date represented by
%%     `Value' into a local date whose timezone is separated from Universal
%%     Coordinated Time (UTC) by `UTCOffset' seconds.
%%   </dd>
%%   <dt>`{timestamp, Value}'</dt>
%%   <dd>
%%     `Value' is encoded to a CBOR integer or floating point number tagged as
%%     an epoch-based datetime. `Value' must be of type
%%     `cbor_time:datetime/0'.  of type `erlang:timestamp()'.
%%   </dd>
%%   <dt>`{Tag, Value}'</dt>
%%   <dd>
%%     `Value' is encoded to a tagged CBOR value. `Tag' must be a positive
%%     integer.
%%   </dd>
%% </dl>
-spec encode(term()) -> iodata().
encode(Value) when is_integer(Value) ->
  encode_integer(Value);
encode(Value) when is_float(Value) ->
  encode_float(Value);
encode(positive_infinity) ->
  <<16#f9, 16#7c, 16#00>>;
encode(negative_infinity) ->
  <<16#f9, 16#fc, 16#00>>;
encode(nan) ->
  <<16#f9, 16#7e, 16#00>>;
encode(Value) when is_boolean(Value) ->
  encode_boolean(Value);
encode(Value) when is_binary(Value) ->
  encode_binary(Value);
encode(Value) when is_list(Value) ->
  encode_list(Value);
encode(Value) when is_map(Value) ->
  encode_map(Value);
encode(null) ->
  <<16#f6>>;
encode(undefined) ->
  <<16#f7>>;
encode({string, Value}) ->
  encode_string(Value);
encode({datetime, Value}) ->
  encode_datetime(Value);
encode({datetime, Value, Offset}) ->
  encode_datetime(Value, Offset);
encode({timestamp, Value}) ->
  encode_timestamp(Value);
encode({Tag, Value}) when is_integer(Tag) ->
  encode_tagged_value(Tag, Value);
encode(Value) ->
  error({unencodable_value, Value}).

%% @doc Encode an Erlang value and return the representation of the resulting
%% CBOR data item as an hex-encoded string.
%%
%% @see encode/1
-spec encode_hex(term()) -> unicode:chardata().
encode_hex(Value) ->
  Data = iolist_to_binary(encode(Value)),
  cbor_util:binary_to_hex_string(Data).

%% @doc Encode an integer to a signed or unsigned CBOR integer.
-spec encode_integer(integer()) -> iodata().
encode_integer(I) when I > 16#ffffffffffffffff ->
  [<<16#c2>>, encode_binary(cbor_util:unsigned_integer_bytes(I))];
encode_integer(I) when I > 16#ffffffff ->
  <<16#1b, I:64>>;
encode_integer(I) when I > 16#ffff ->
  <<16#1a, I:32>>;
encode_integer(I) when I > 16#ff ->
  <<16#19, I:16>>;
encode_integer(I) when I > 16#17 ->
  <<16#18, I:8>>;
encode_integer(I) when I >= 16#00 ->
  <<I:8>>;
encode_integer(I) when I >= -16#18 ->
  <<(16#20 - 1 - I):8>>;
encode_integer(I) when I >= -16#ff - 1 ->
  <<16#38, (-1 - I):8>>;
encode_integer(I) when I >= -16#ffff - 1 ->
  <<16#39, (-1 - I):16>>;
encode_integer(I) when I >= -16#ffffffff - 1 ->
  <<16#3a, (-1 - I):32>>;
encode_integer(I) when I >= -16#ffffffffffffffff - 1 ->
  <<16#3b, (-1 - I):64>>;
encode_integer(I) ->
  [<<16#c3>>, encode_binary(cbor_util:unsigned_integer_bytes(-1 - I))].

%% @doc Encode a float to a CBOR floating point number.
%%
%% We currently do not support encoding to 16 bit or 32 bit CBOR floating
%% point numbers.
-spec encode_float(float()) -> iodata().
encode_float(F) ->
  <<16#fb, F:64/float>>.

%% @doc Encode a boolean to a CBOR boolean.
-spec encode_boolean(boolean()) -> iodata().
encode_boolean(false) ->
  <<16#f4>>;
encode_boolean(true) ->
  <<16#f5>>.

%% @doc Encode binary data to a CBOR byte string.
-spec encode_binary(binary()) -> iodata().
encode_binary(Bin) ->
  [cbor_util:encode_sequence_header(2, byte_size(Bin)), Bin].

%% @doc Encode a binary string to a CBOR text string.
-spec encode_string(unicode:chardata()) -> iodata().
encode_string(CharData) ->
  Bin = unicode:characters_to_binary(CharData),
  [cbor_util:encode_sequence_header(3, byte_size(Bin)), Bin].

%% @doc Encode a list to a CBOR array.
-spec encode_list(list()) -> iodata().
encode_list(List) ->
  {Data, Len} = encode_list_data(List, <<>>, 0),
  [cbor_util:encode_sequence_header(4, Len), Data].

-spec encode_list_data(list(), iodata(), Len) -> {iodata(), Len} when
    Len :: non_neg_integer().
encode_list_data([], Data, Len) ->
  {Data, Len};
encode_list_data([Value | Rest], Data, Len) ->
  encode_list_data(Rest, [Data, encode(Value)], Len + 1).

%% @doc Encode a map to a CBOR map.
%%
%% Note that we have to flatten key data to sort pairs by lexical byte order,
%% in order to follow the rules for canonical encoding.
-spec encode_map(map()) -> iodata().
encode_map(Map) ->
  Len = maps:size(Map),
  Data = maps:fold(fun (K, V, Acc) ->
                       [[iolist_to_binary(encode(K)), encode(V)] | Acc]
                   end, [], Map),
  SortedData = lists:sort(fun ([K1, _], [K2, _]) ->
                              K1 =< K2
                          end, Data),
  [cbor_util:encode_sequence_header(5, Len), SortedData].

%% @doc Encode a datetime value to a CBOR tagged string.
-spec encode_datetime(Datetime) -> iodata() when
    Datetime :: calendar:datetime() | integer().
encode_datetime(Datetime) ->
  encode_datetime(Datetime, 0).

%% @doc Encode a datetime value to a CBOR tagged string with a specific
%% timezone offset.
-spec encode_datetime(cbor_time:datetime(), integer()) -> iodata().
encode_datetime(Datetime, Offset) ->
  {Seconds, _Nanoseconds} = cbor_time:datetime_to_seconds(Datetime),
  OffsetValue = case Offset of
                  0 -> "Z";
                  _ -> Offset
                end,
  String = calendar:system_time_to_rfc3339(Seconds, [{offset, OffsetValue}]),
  encode_tagged_value(0, {string, String}).

%% @doc Encode a datetime value to a CBOR integer or floating point number
%% representing an epoch-based date. An integer is used when the datetime
%% value does not have fractional seconds.
-spec encode_timestamp(cbor_time:datetime()) -> iodata().
encode_timestamp(Datetime) ->
  case cbor_time:datetime_to_seconds(Datetime) of
    {Seconds, 0} ->
      encode_tagged_value(1, Seconds);
    {Seconds, Nanoseconds} ->
      encode_tagged_value(1, erlang:float(Seconds) + Nanoseconds * 1.0e-9)
  end.

%% @doc Encode a value preceded by a semantic tag.
-spec encode_tagged_value(tag(), term()) -> iodata().
encode_tagged_value(Tag, Value) when Tag =< 16#17 ->
  [<<6:3, Tag:5>>, encode(Value)];
encode_tagged_value(Tag, Value) when Tag =< 16#ff ->
  [<<6:3, 24:5, Tag:8>>, encode(Value)];
encode_tagged_value(Tag, Value) when Tag =< 16#ffff ->
  [<<6:3, 25:5, Tag:16>>, encode(Value)];
encode_tagged_value(Tag, Value) when Tag =< 16#ffffffff ->
  [<<6:3, 26:5, Tag:32>>, encode(Value)];
encode_tagged_value(Tag, Value) when Tag =< 16#ffffffffffffffff ->
  [<<6:3, 27:5, Tag:64>>, encode(Value)];
encode_tagged_value(Tag, _Value) ->
  error({unencodable_tag, Tag}).

%% @doc Decode a CBOR data item from binary data and return both the Erlang
%% value it represents and the rest of the binary data which were not decoded.
%%
%% @see decode/2
-spec decode(iodata()) -> decoding_result(term()).
decode(Data) ->
  decode(Data, default_decoding_options()).

%% @doc Decode a CBOR data item from binary data and return both the Erlang
%% value it represents and the rest of the binary data which were not decoded.
%%
%% CBOR values are converted to Erlang values as follows:
%% <ul>
%%   <li>
%%     CBOR integers, unsigned integers, and negative integers are converted
%%     to Erlang integers.
%%   </li>
%%   <li>
%%     CBOR byte strings are converted to Erlang binaries.
%%   </li>
%%   <li>
%%     CBOR UTF-8 strings are validated and converted to Erlang binaries.
%%   </li>
%%   <li>
%%     CBOR arrays are converted to Erlang lists.
%%   </li>
%%   <li>
%%     CBOR maps are converted to Erlang maps.
%%   </li>
%%   <li>
%%     CBOR tagged values are converted either to tuples of the form `{Tag,
%%     Value}' or, for specific tags, to the following Erlang values:
%%     <dl>
%%      <dt>0 (standard datetime)</dt>
%%      <dd>
%%        TODO standard datetime
%%      </dd>
%%      <dt>1 (epoch-based datetime)</dt>
%%      <dd>
%%        TODO epoch-based datetime
%%      </dd>
%%      <dt>2 (positive bignum)</dt>
%%      <dt>3 (negative bignum)</dt>
%%      <dd>
%%        An Erlang integer.
%%      </dd>
%%      <dt>24 (CBOR data)</dt>
%%      <dd>
%%        An Erlang value formed by decoding the CBOR-encoded byte string.
%%      </dd>
%%      <dt>32 (URI)</dt>
%%      <dd>
%%        An Erlang binary string containing the URI. While it would be
%%        possible to parse the URI string, using for example the `uri_string'
%%        module, it would not be practical since most functions using URIs
%%        expect the textual representation.
%%      </dd>
%%      <dt>33 (base64url-encoded data)</dt>
%%      <dd>
%%        An Erlang binary formed by decoding the base64url-encoded UTF-8
%%        string.
%%      </dd>
%%      <dt>34 (base64-encoded data)</dt>
%%      <dd>
%%        An Erlang binary formed by decoding the base64-encoded UTF-8 string.
%%      </dd>
%%      <dt>35 (regular expression)</dt>
%%      <dd>
%%        An Erlang binary string containing the regular expression.
%%      </dd>
%%      <dt>36 (MIME message)</dt>
%%      <dd>
%%        An Erlang binary string containing the MIME message.
%%      </dd>
%%      <dt>55799 (CBOR value)</dt>
%%      <dd>
%%        An Erlang value formed by decoding the tagged value.
%%      </dd>
%%     </dl>
%%     We do not interpret:
%%     <ul>
%%       <li>
%%         decimal fractions and big floats, because Erlang do not have data
%%         types to store them;
%%       </li>
%%       <li>
%%         expected encoding to base64url, base64 and base16 (tags 21 to 23)
%%         because the resulting encoded data would be ambiguous.
%%       </li>
%%     </ul>
%%   </li>
%%   <li>
%%     CBOR simple values are converted either to tuples of the form
%%     `{simple_value, Integer}' or, for specific numeric values, to the
%%     following Erlang values:
%%     <dl>
%%      <dt>20</dt><dd>`false'</dd>
%%      <dt>21</dt><dd>`true'</dd>
%%      <dt>22</dt><dd>`null'</dd>
%%      <dt>23</dt><dd>`undefined'</dd>
%%     </dl>
%%   </li>
%%   <li>
%%     CBOR floats are converted to Erlang floats. NaN is converted to the
%%     `nan' atom. Positive and negative infinity values are converted
%%     respectively to the `positive_infinity' and `negative_infinity' atoms.
%%   </li>
%% </ul>
-spec decode(iodata(), decoding_options()) ->
        {ok, term(), iodata()} | {error, term()}.
decode(<<Type:8, Data/binary>>, _Opts) when Type =< 16#17 ->
  {ok, Type, Data};
decode(<<Type:8, Data/binary>>, _Opts) when Type >= 16#18, Type =< 16#1b ->
  decode_unsigned_integer(Type, Data);
decode(<<Type:8, Data/binary>>, _Opts) when Type >= 16#20, Type =< 16#37 ->
  {ok, -1 - (Type - 16#20), Data};
decode(<<Type:8, Data/binary>>, _Opts) when Type >= 16#38, Type =< 16#3b ->
  decode_negative_integer(Type, Data);
decode(<<Type:8, Data/binary>>, _Opts) when Type >= 16#40, Type =< 16#5b ->
  decode_byte_string(Type, Data);
decode(<<16#5f:8, Data/binary>>, _Opts) ->
  decode_indefinite_length_byte_string(Data);
decode(<<Type:8, Data/binary>>, _Opts) when Type >= 16#60, Type =< 16#7b ->
  decode_utf8_string(Type, Data);
decode(<<16#7f:8, Data/binary>>, _Opts) ->
  decode_indefinite_length_utf8_string(Data);
decode(<<Type:8, Data/binary>>, _Opts) when Type >= 16#80, Type =< 16#9b ->
  decode_array(Type, Data);
decode(<<16#9f:8, Data/binary>>, _Opts) ->
  decode_indefinite_length_array(Data);
decode(<<Type:8, Data/binary>>, _Opts) when Type >= 16#a0, Type =< 16#bb ->
  decode_map(Type, Data);
decode(<<16#bf:8, Data/binary>>, _Opts) ->
  decode_indefinite_length_map(Data);
decode(<<Type:8, Data/binary>>, Opts) when Type >= 16#c0, Type =< 16#db ->
  decode_tagged_value(Type, Data, Opts);
decode(<<Type:8, Data/binary>>, _Opts) when Type >= 16#e0, Type =< 16#f8 ->
  decode_simple_value(Type, Data);
decode(<<Type, Data/binary>>, _Opts) when Type >= 16#f9, Type =< 16#fb ->
  decode_float(Type, Data);
decode(<<Type:8, _Data/binary>>, _Opts) ->
  {error, {invalid_type_tag, Type}}.

%% @doc Decode a CBOR data item from an hex-encoded string and return both the
%% Erlang value it represents and the rest of the string which was not
%% decoded.
%%
%% @see decode/1
-spec decode_hex(string()) -> decoding_result(term()).
decode_hex(Value) ->
  decode_hex(Value, default_decoding_options()).

%% @doc Decode a CBOR data item from an hex-encoded string and return both the
%% Erlang value it represents and the rest of the string which was not
%% decoded.
%%
%% @see decode/2
-spec decode_hex(string(), decoding_options()) -> decoding_result(term()).
decode_hex(Str, Opts) ->
  Bin = cbor_util:hex_string_to_binary(Str),
  case decode(Bin, Opts) of
    {ok, Value, Rest} ->
      {ok, Value, cbor_util:binary_to_hex_string(Rest)};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode a CBOR unsigned integer.
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

%% @doc Decode a CBOR negative integer.
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

%% @doc Decode a CBOR binary string to an Erlang binary.
-spec decode_byte_string(Type, iodata()) -> decoding_result(binary()) when
    Type :: 16#40..16#5b.
decode_byte_string(Type, Data) ->
  {Len, Data2} = cbor_util:decode_sequence_header(Type, Data),
  case Data2 of
    <<Bin:Len/binary, Rest/binary>> ->
      {ok, iolist_to_binary(Bin), Rest};
    _ ->
      {error, truncated_byte_string}
  end.

%% @doc Decode a CBOR binary string without an explicite length to an Erlang
%% binary.
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

%% @doc Decode a CBOR UTF-8 string to an Erlang binary. An invalid or
%% incomplete UTF-8 sequence causes a decoding error.
-spec decode_utf8_string(Type, iodata()) -> decoding_result(binary()) when
    Type :: 16#60..16#7b.
decode_utf8_string(Type, Data) ->
  {Len, Data2} = cbor_util:decode_sequence_header(Type, Data),
  case Data2 of
    <<Bin:Len/binary, Rest/binary>> ->
      Str = case unicode:characters_to_binary(Bin) of
              Bin2 when is_binary(Bin2) ->
                Bin2;
              {error, _, _} ->
                {error, {invalid_utf8_string, Bin}};
              {incomplete, _, _} ->
                {error, {incomplete_utf8_string, Bin}}
            end,
      {ok, Str, Rest};
    _ ->
      {error, truncated_utf8_string}
  end.

%% @doc Decode a CBOR UTF-8 string without an explicite length to an Erlang
%% binary. An invalid or incomplete UTF-8 sequence causes a decoding error.
-spec decode_indefinite_length_utf8_string(iodata()) ->
        decoding_result(binary()).
decode_indefinite_length_utf8_string(Data) ->
  case binary:match(Data, <<255:8>>) of
    {Off, _Len} ->
      <<Bin:Off/binary, 255:8, Rest/binary>> = Data,
      Str = case unicode:characters_to_binary(Bin) of
              Bin2 when is_binary(Bin2) ->
                Bin2;
              {error, _, _} ->
                {error, {invalid_utf8_string, Bin}};
              {incomplete, _, _} ->
                {error, {incomplete_utf8_string, Bin}}
            end,
      {ok, Str, Rest};
    nomatch ->
      {error, truncated_utf8_string}
  end.

%% @doc Decode a CBOR array to an Erlang list.
-spec decode_array(Type, iodata()) -> decoding_result(list()) when
    Type :: 16#80..16#9b.
decode_array(Type, Data) ->
  {Len, Data2} = cbor_util:decode_sequence_header(Type, Data),
  case decode_values(Data2, Len, []) of
    {ok, Values, Rest} ->
      {ok, Values, Rest};
    {error, truncated_sequence} ->
      {error, truncated_array};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode a CBOR array without an explicite length to an Erlang list.
-spec decode_indefinite_length_array(iodata()) -> decoding_result(list()).
decode_indefinite_length_array(Data) ->
  case decode_indefinite_length_values(Data, []) of
    {ok, Values, Rest} ->
      {ok, Values, Rest};
    {error, truncated_sequence} ->
      {error, truncated_array};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode a CBOR map to an Erlang map.
-spec decode_map(Type, iodata()) -> decoding_result(map()) when
    Type :: 16#a0..16#bb.
decode_map(Type, Data) ->
  {Len, Data2} = cbor_util:decode_sequence_header(Type, Data),
  case decode_values(Data2, Len*2, []) of
    {ok, Values, Rest} ->
      {ok, cbor_util:list_to_map(Values), Rest};
    {error, truncated_sequence} ->
      {error, truncated_map};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode a CBOR map without an explicite length to an Erlang map.
-spec decode_indefinite_length_map(iodata()) -> decoding_result(map()).
decode_indefinite_length_map(Data) ->
  case decode_indefinite_length_values(Data, []) of
    {ok, Values, _Rest} when (length(Values) rem 2) /= 0 ->
      {error, odd_number_of_map_values};
    {ok, Values, Rest} ->
      {ok, cbor_util:list_to_map(Values), Rest};
    {error, truncated_sequence} ->
      {error, truncated_map};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode a CBOR tagged value. If the tag is supported, return a suitable
%% Erlang value; if it is not, return a tuple of the form `{Tag, Value}'.
-spec decode_tagged_value(Type, iodata(), Opts) -> decoding_result(Result) when
    Type :: 16#c0..16#db,
    Opts :: decoding_options(),
    Result :: tagged_value() | term().
decode_tagged_value(Type, Data, Opts) when Type >= 16#c0, Type =< 16#d7 ->
  decode_tagged_data(Type - 16#c0, Data, Opts);
decode_tagged_value(16#d8, <<Tag:8, Data/binary>>, Opts) ->
  decode_tagged_data(Tag, Data, Opts);
decode_tagged_value(16#d9, <<Tag:16, Data/binary>>, Opts) ->
  decode_tagged_data(Tag, Data, Opts);
decode_tagged_value(16#da, <<Tag:32, Data/binary>>, Opts) ->
  decode_tagged_data(Tag, Data, Opts);
decode_tagged_value(16#db, <<Tag:64, Data/binary>>, Opts) ->
  decode_tagged_data(Tag, Data, Opts);
decode_tagged_value(_Type, _Data, _Opts) ->
  {error, truncated_tagged_value}.

-spec decode_tagged_data(tag(), iodata(), decoding_options()) ->
        decoding_result(Result) when
    Result :: tagged_value() | term().
decode_tagged_data(Tag, Data, Opts) ->
  case decode(Data) of
    {ok, Value, Rest} ->
      case interpret_tagged_value({Tag, Value}, Opts) of
        {ok, Value2} ->
          {ok, Value2, Rest};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec interpret_tagged_value(tagged_value(), decoding_options()) ->
        interpretation_result(term()).
interpret_tagged_value(TaggedValue = {Tag, _Value},
                       #{tagged_value_interpreters := Interpreters}) ->
  case maps:find(Tag, Interpreters) of
    {ok, Interpreter} ->
      Interpreter(TaggedValue);
    error ->
      {ok, TaggedValue}
  end;
interpret_tagged_value(TaggedValue, _Opts) ->
  {ok, TaggedValue}.

%% @doc Interpret a value by returning it without any transformation.
-spec interpret_value(tagged_value()) -> interpretation_result(term()).
interpret_value({_Tag, Value}) ->
  {ok, Value}.

%% @doc Interpret a CBOR positive bignum by converting it to an Erlang
%% integer.
-spec interpret_positive_bignum(tagged_value()) ->
        interpretation_result(integer()).
interpret_positive_bignum({_Tag, Value}) when is_binary(Value) ->
  Size = byte_size(Value) * 8,
  <<N:Size>> = Value,
  {ok, N};
interpret_positive_bignum({_Tag, Value}) ->
  {error, {invalid_bignum_value, Value}}.

%% @doc Interpret a CBOR negative bignum by converting it to an Erlang
%% integer.
-spec interpret_negative_bignum(tagged_value()) ->
        interpretation_result(integer()).
interpret_negative_bignum({_Tag, Value}) when is_binary(Value) ->
  Size = byte_size(Value) * 8,
  <<N:Size>> = Value,
  {ok, -1 - N};
interpret_negative_bignum({_Tag, Value}) ->
  {error, {invalid_bignum_value, Value}}.

%% @doc Interpret a base64url-encoded byte string by decoding it.
-spec interpret_base64url_data(tagged_value()) ->
        interpretation_result(binary()).
interpret_base64url_data({_Tag, Value}) when is_binary(Value) ->
  case cbor_base64url:decode(Value) of
    {ok, Bin} ->
      {ok, Bin};
    {error, Reason} ->
      {error, {invalid_base64url_data, Reason}}
  end;
interpret_base64url_data({_Tag, Value}) ->
  {error, {invalid_base64url_data_value, Value}}.

%% @doc Interpret a base64-encoded byte string by decoding it.
-spec interpret_base64_data(tagged_value()) -> interpretation_result(binary()).
interpret_base64_data({_Tag, Value}) when is_binary(Value) ->
  case cbor_base64:decode(Value) of
    {ok, Bin} ->
      {ok, Bin};
    {error, Reason} ->
      {error, {invalid_base64_data, Reason}}
  end;
interpret_base64_data({_Tag, Value}) ->
  {error, {invalid_base64_data_value, Value}}.

%% @doc Interpret a CBOR-encoded value by decoding it.
-spec interpret_cbor_value(tagged_value()) -> interpretation_result(term()).
interpret_cbor_value({_Tag, Value}) when is_binary(Value) ->
  case cbor:decode(Value) of
    {ok, Value2, <<>>} ->
      {ok, Value2};
    {ok, _Value2, Rest} ->
      {error, {invalid_trailing_data, Rest}};
    {error, Reason} ->
      {error, {invalid_cbor_data, Reason}}
  end;
interpret_cbor_value({_Tag, Value}) ->
  {error, {invalid_cbor_data_value, Value}}.

%% @doc Decode a CBOR simple value. Numeric simple values are decoded to
%% Erlang integers. Others are decoded to Erlang atoms.
-spec decode_simple_value(Type, iodata()) -> decoding_result(simple_value()) when
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

%% @doc Decode a CBOR floating point number to an Erlang float. We have to
%% manually decode half-precision floating point numbers because the Erlang
%% binary syntax does not support them.
-spec decode_float(Type, iodata()) -> decoding_result(float_value()) when
    Type :: 16#f9..16#fb.
%% Half-precision
decode_float(16#f9, <<_S:1, 0:5, 0:10, Rest/binary>>) ->
  {ok, 0.0, Rest};
decode_float(16#f9, <<S:1, 0:5, F:10, Rest/binary>>) ->
  Value = math:pow(-1.0, S) * math:pow(2.0, -14) * (F/1024.0),
  {ok, Value, Rest};
decode_float(16#f9, <<0:1, 31:5, 0:10, Rest/binary>>) ->
  {ok, positive_infinity, Rest};
decode_float(16#f9, <<1:1, 31:5, 0:10, Rest/binary>>) ->
  {ok, negative_infinity, Rest};
decode_float(16#f9, <<_S:1, 31:5, _F:10, Rest/binary>>) ->
  {ok, nan, Rest};
decode_float(16#f9, <<S:1, E:5, F:10, Rest/binary>>) ->
  Value = math:pow(-1.0, S) * math:pow(2.0, E-15) * (1 + F/1024.0),
  {ok, Value, Rest};
%% Single precision
decode_float(16#fa, <<_S:1, 0:8, 0:23, Rest/binary>>) ->
  {ok, 0.0, Rest};
decode_float(16#fa, <<0:1, 255:8, 0:23, Rest/binary>>) ->
  {ok, positive_infinity, Rest};
decode_float(16#fa, <<1:1, 255:8, 0:23, Rest/binary>>) ->
  {ok, negative_infinity, Rest};
decode_float(16#fa, <<_S:1, 255:8, _F:23, Rest/binary>>) ->
  {ok, nan, Rest};
decode_float(16#fa, <<F:32/float, Rest/binary>>) ->
  {ok, F, Rest};
%% Double precision
decode_float(16#fb, <<_S:1, 0:11, 0:52, Rest/binary>>) ->
  {ok, 0.0, Rest};
decode_float(16#fb, <<0:1, 2047:11, 0:52, Rest/binary>>) ->
  {ok, positive_infinity, Rest};
decode_float(16#fb, <<1:1, 2047:11, 0:52, Rest/binary>>) ->
  {ok, negative_infinity, Rest};
decode_float(16#fb, <<_S:1, 2047:11, _F:52, Rest/binary>>) ->
  {ok, nan, Rest};
decode_float(16#fb, <<F:64/float, Rest/binary>>) ->
  {ok, F, Rest};
%% Truncated
decode_float(_Type, _Data) ->
  {error, truncated_float}.

%% @doc Decode a fixed number of consecutive CBOR data items and return them
%% as a list.
-spec decode_values(iodata(), non_neg_integer(), list()) ->
        decoding_result(list()).
decode_values(Data, 0, Acc) ->
  {ok, lists:reverse(Acc), Data};
decode_values(<<>>, _N, _Acc) ->
  {error, truncated_sequence};
decode_values(Data, N, Acc) ->
  case decode(Data) of
    {ok, Value, Rest} ->
      decode_values(Rest, N-1, [Value | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Decode multiple consecutive CBOR data items until a break tag is
%% found and return them as a list.
-spec decode_indefinite_length_values(iodata(), list()) ->
        decoding_result(list()).
decode_indefinite_length_values(<<>>, _Acc) ->
  {error, truncated_sequence};
decode_indefinite_length_values(<<16#ff:8, Data/binary>>, Acc) ->
  {ok, lists:reverse(Acc), Data};
decode_indefinite_length_values(Data, Acc) ->
  case decode(Data) of
    {ok, Value, Rest} ->
      decode_indefinite_length_values(Rest, [Value | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.
