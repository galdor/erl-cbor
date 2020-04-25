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

-export([encode/1, decode/1]).

%% @doc Encode an Erlang value and return the binary representation of the
%% resulting CBOR data item.
%%
%% Integers, floats, boolean, binaries, lists and maps are encoded to the
%% associated CBOR type.
%%
%% Atoms are used for specific constants:
%% <dl>
%%   <dt>infinity</dt>
%%   <dt>positive_infinity</dt>
%%   <dd>IEEE.754 positive infinity.</dd>
%%
%%   <dt>negative_infinity</dt>
%%   <dd>IEEE.754 negative infinity.</dd>
%%
%%   <dt>nan</dt>
%%   <dd>IEEE.754 NaN.</dd>
%%
%%   <dt>null</dt>
%%   <dd>CBOR null value.</dd>
%%
%%   <dt>undefined</dt>
%%   <dd>CBOR undefined value.</dd>
%% </dl>
%%
%% Tuples of the form `{Type, Value}' are used for more complex CBOR
%% values. If `Type' is a positive integer, `Value' is encoded to a tagged
%% CBOR value. If `Type' is an atom, the behaviour depends on its value:
%% <dl>
%%   <dt>string</dt>
%%   <dd>
%%     `Value' is encoded to a CBOR string; `Value' must be of type
%%     `unicode:chardata/0'.
%%   </dd>
%% </dl>
-spec encode(term()) -> iodata().
encode(Value) when is_integer(Value) ->
  encode_integer(Value);
encode(Value) when is_float(Value) ->
  encode_float(Value);
encode(infinity) ->
  <<16#f9, 16#7c, 16#00>>;
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
encode({Tag, Value}) when is_integer(Tag) ->
  encode_tagged_value(Tag, Value);
encode(Value) ->
  error({unencodable_value, Value}).

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
  [cbor_util:sequence_header(2, byte_size(Bin)), Bin].

%% @doc Encode a binary string to a CBOR text string.
-spec encode_string(unicode:chardata()) -> iodata().
encode_string(CharData) ->
  Bin = unicode:characters_to_binary(CharData),
  [cbor_util:sequence_header(3, byte_size(Bin)), Bin].

%% @doc Encode a list to a CBOR array.
-spec encode_list(list()) -> iodata().
encode_list(List) ->
  {Data, Len} = encode_list_data(List, <<>>, 0),
  [cbor_util:sequence_header(4, Len), Data].

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
  [cbor_util:sequence_header(5, Len), SortedData].

%% @doc Encode a value preceded by a semantic tag.
-spec encode_tagged_value(non_neg_integer(), term()) -> iodata().
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
-spec decode(Data) -> {ok, Value, Rest} | {error, Reason} when
    Data :: iodata(),
    Value :: term(),
    Rest :: iodata(),
    Reason :: term().
decode(_Data) ->
  %% TODO
  error("unimplemented").
