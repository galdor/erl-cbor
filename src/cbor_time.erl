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

-module(cbor_time).

-export_type([utc_offset/0]).

-export([utc_offset_seconds/1]).

-type utc_offset() :: integer()
                    | {0..23} | {-23..0}
                    | {0..23, 0..59} | {-23..0, -59..0}.
%% A time difference between a local time and Coordinated Universal Time
%% (UTC).

%% @doc Convert the representation of a UTC offset to a number of seconds.
-spec utc_offset_seconds(utc_offset()) -> integer().
utc_offset_seconds(Seconds) when is_integer(Seconds) ->
  Seconds;
utc_offset_seconds({Hours}) ->
  Hours * 3600;
utc_offset_seconds({Hours, Minutes}) ->
  Hours * 3600 + Minutes * 60.
