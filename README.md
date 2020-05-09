# Project
This repository contains an Erlang library implementing the CBOR data encoding
format defined in [RFC 7049](https://tools.ietf.org/html/rfc7049).

In the current state, the library supports:

- Encoding of various kinds of Erlang values, with special constructions for
  tagged values.
- Decoding of all CBOR value types, including indefinite length sequences.
- Interpretation of various tagged data to suitable Erlang values.
- A way to customize how tagged CBOR values are interpreted to Erlang values.

# Documentation
Code documentation is available [online](https://galdor.github.io/erl-cbor/doc/).

# Contact
If you find a bug or have any question, feel free to open a GitHub issue or to
contact me [by email](mailto:khaelin@gmail.com).

Please note that I do not currently review or accept any contribution.
