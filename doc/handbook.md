% erl-cbor

# Introduction
The erl-cbor project is an Erlang library implementing the CBOR data encoding
format defined in [RFC 7049](https://tools.ietf.org/html/rfc7049).

# Encoding
The `cbor:encode/1` function encodes an Erlang term to a CBOR value and
returns it as binary data.

Example: `cbor:encode([1, 2, 3])` returns `[<<131>>,[[[<<>>,<<1>>],<<2>>],<<3>>]]`.

The `cbor:encode_hex/1` function encodes data as `cbor:encode/1` but returns
the result as a hex-encoded binary string.

Example: `cbor:encode_hex([1, 2, 3])` returns `<<"83010203">>`.

## Type mapping
Erlang data are encoded using the following type mapping:

- Integers are encoded as CBOR integers if their value allows it. Values which
  are too small or too large for CBOR integers are encoded as positive or
  negative tagged big numbers.
- Floats are encoded as CBOR floating point numbers. Note that because Erlang
  does not handle the negative zero IEEE.754 value internally (`-0` is parsed
  to `0`), the encoder will never produce a negative zero CBOR floating point
  value for an Erlang float; the `negative_zero` atom can be used instead.
- Booleans are encoded using the `true` and `false` CBOR simple values.
- Binaries are encoded as CBOR byte strings.
- Lists are encoded as CBOR arrays.
- Maps are encoded as CBOR maps.
- Atoms are used for specific CBOR values:
  - `positive_infinity`: the IEEE.754 +Inf floating point value.
  - `negative_infinity`: the IEEE.754 -Inf floating point value.
  - `positive_zero`: the IEEE.754 +0.0 floating point value.
  - `negative_zero`: the IEEE.754 -0.0 floating point value.
  - `nan`: the IEEE.754 NaN floating point value.
  - `null`: the `null` CBOR simple value.
  - `undefined`: the `undefined` CBOR simple value.
- Tuples are used for more complex CBOR values:
  - `{string, Value}`: `Value` is encoded to a CBOR UTF-8 string; `Value` must
    be of type `unicode:chardata/0`.
  - `{datetime, Value}`: `Value` is encoded to a CBOR UTF-8 string tagged as a
    standard datetime string using the datetime format specified by
    RFC 3339. `Value` must be of type `cbor_time:datetime/0`.
  - `{datetime, Value, UTCOffset}`: `Value` is encoded to a CBOR UTF-8 string
    tagged as a standard datetime string using the datetime format specified
    by RFC 3339. `Value` must be of type `cbor_time:datetime/0`. `UTCOffset`
    is used to transform the universal date represented by `Value` into a
    local date whose timezone is separated from Universal Coordinated Time
    (UTC) by `UTCOffset` seconds.
  - `{timestamp, Value}`: `Value` is encoded to a CBOR integer or floating
    point number tagged as an epoch-based datetime. `Value` must be either of
    type `cbor_time:datetime/0` or of type `erlang:timestamp()`.
  - `{Tag, Value}`: `Value` is encoded to a tagged CBOR value; `Tag` must be a
    positive integer.

Encoding functions will raise an error of the form `{unencodable_value,
Value}` if the value cannot be mapped to a CBOR data type.

Note that the encoder does not support indefinite length byte strings, UTF-8
strings, arrays or maps.

# Decoding
The `cbor:decode/1` function decodes a single CBOR value from binary data. If
it succeeds, it returns a tuple of the form `{ok, Value, Rest}` where `Value`
is an Erlang term representing the decoded value, and `Rest` are the binary
data left after decoding. If the function fails, it returns the usual `{error,
Reason}` tuple.

Example: `cbor:decode(<<99, 97, 98, 99, 24, 42>>)` returns
`{ok,<<"abc">>,<<24,42>>}`.

The `cbor:decode_hex/1` function decodes a CBOR value as `cbor:decode/1` but
uses a hex-encoded binary or character string.

Example: `cbor:decode_hex(<<"63616263182a">>)` returns
`{ok,<<"abc">>,"182a"}`.

The `cbor:decode/2` and `cbor:decode_hex/2` functions expect a map of decoding
options as second argument.

## Decoding options
Decoding options are represented by a map. The following options are
supported:

- `max_depth`: the maximum depth supported by the decoder; reaching this limit
  will make decoding fail with a `max_deph_reached` error. Arrays, maps and
  tagged values increment the current depth during decoding. The default limit
  is 1024.
- `tagged_value_interpreters`: a map containing a tagged value interpreter
  function for each supported tagged value. Unsupported tagged values will be
  decoded to a tuple of the form `{Tag, Value}`.

The `cbor_decoding:default_options/0` function returns the map of options used
by default.

## Type mapping
CBOR values are converted to Erlang terms as follows:

- CBOR integers, unsigned integers, and negative integers are converted to
  Erlang integers.
- CBOR byte strings are converted to Erlang binaries.
- CBOR UTF-8 strings are validated and converted to Erlang binaries.
- CBOR arrays are converted to Erlang lists.
- CBOR maps are converted to Erlang maps.
- CBOR tagged values are converted either to tuples of the form `{Tag, Value}`
  or, for specific tags, to the following Erlang values:

  - 0 (standard datetime): a RFC3339-formatted binary string.
  - 1 (epoch-based datetime): a nanosecond epoch-based timestamp.
  - 2 (positive bignum): an Erlang integer.
  - 3 (negative bignum): an Erlang integer.
  - 24 (CBOR data): an Erlang value formed by decoding the CBOR-encoded byte
    string.
  - 32 (URI) An Erlang binary string containing the URI. While it would be
    possible to parse the URI string, using for example the `uri_string`
    module, it would not be practical since most functions using URIs expect
    the textual representation.
  - 33 (base64url-encoded data): an Erlang binary formed by decoding the
    base64url-encoded UTF-8 string.
  - 34 (base64-encoded data): an Erlang binary formed by decoding the
     base64-encoded UTF-8 string.
  - 35 (regular expression): an Erlang binary string containing the regular
    expression.
  - 36 (MIME message): an Erlang binary string containing the MIME message.
  - 55799 (CBOR value): an Erlang value formed by decoding the tagged value.

  We do not interpret:
  - decimal fractions and big floats, because Erlang do not have data types to
    store them;
  - expected encoding to base64url, base64 and base16 (tags 21 to 23) because
    the resulting encoded data would be ambiguous.
- CBOR simple values are converted either to tuples of the form
  `{simple_value, Integer}` or, for specific numeric values, to the following
  Erlang values:
  - 20: `false`
  - 21: `true`
  - 22: `null`
  - 23: `undefined`
- CBOR floats are converted to Erlang floats. NaN is converted to the `nan`
  atom. Positive and negative infinity values are converted respectively to
  the `positive_infinity` and `negative_infinity` atoms.

## Tagged value interpreters
Once decoded, tagged values can be interpreted to specific Erlang values. For
example, values with the tag 2 (positive bignums) are interpreted as Erlang
integers.

This mapping is configured with a map which associates each tag to its
interpreter. Interpreters are functions which take the `{Tag, Value}` tagged
value as argument and return either `{ok, Value2}` or `{error, Reason}`.

The `cbor_decoding:default_tagged_value_interpreters/0` function returns the
default map of tagged value interpreters.

The following example extends the default interpreter map to decode CBOR
URIs (tag 0) to `uri_string:uri_map()` maps:

```erlang
interpret_uri(_Decoder, {_Tag, Value}) when is_binary(Value) ->
  case uri_string:parse(Value) of
    Map when is_map(Map) ->
      {ok, Map};
    {error, Reason, Datum} ->
      {error, {Reason, Datum}}
  end;
interpret_uri(_Decoder, TaggedValue) ->
  {error, {invalid_tagged_value, TaggedValue}}.

custom_decode_hex(Data) ->
  Interpreters = maps:merge(cbor_decoding:default_tagged_value_interpreters(),
                            #{32 => fun interpret_uri/2}),
  Opts = maps:merge(cbor_decoding:default_options(),
                    #{tagged_value_interpreters => Interpreters}),
  cbor:decode(Data, Opts).
```
