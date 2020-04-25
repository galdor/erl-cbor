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

-export([unsigned_integer_bytes/1]).

%% @doc Return the binary representation of an unsigned integer of
%% undetermined size.
-spec unsigned_integer_bytes(non_neg_integer()) -> binary().
unsigned_integer_bytes(I) ->
  unsigned_integer_bytes(I, []).

-spec unsigned_integer_bytes(non_neg_integer(), [byte()]) -> binary().
unsigned_integer_bytes(0, Acc) ->
  list_to_binary(Acc);
unsigned_integer_bytes(I, Acc) ->
  unsigned_integer_bytes(I bsr 8, [I band 16#ff | Acc]).
