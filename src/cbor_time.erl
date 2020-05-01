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

-export_type([datetime/0]).

-export([datetime_to_seconds/1]).

-type datetime() :: integer()
                  | {integer(), erlang:time_unit()}
                  | erlang:timestamp()
                  | calendar:datetime().
%% A representation of a date and time which encompasses the most common
%% Erlang representations:
%% <ul>
%%   <li>An epoch-based timestamp in seconds.</li>
%%   <li>An epoch-based timestamp with an explicit time unit.</li>
%%   <li>An Erlang timestamp represented as a tuple of megaseconds, seconds
%%   and microseconds.</li>
%%   <li>A calendar date represented as a tuple of date and time.</li>
%% </ul>

%% @doc Convert a datetime to an epoch based timestamp represented by a tuple
%% of seconds and nanoseconds.
-spec datetime_to_seconds(datetime()) -> {integer(), integer()}.
datetime_to_seconds(Seconds) when is_integer(Seconds) ->
  {Seconds, 0};
datetime_to_seconds({Seconds, second}) ->
  {Seconds, 0};
datetime_to_seconds({Milliseconds, millisecond}) ->
  Seconds = Milliseconds div 1000,
  {Seconds, (Milliseconds rem 1000) * 1000000};
datetime_to_seconds({Microseconds, microsecond}) ->
  Seconds = Microseconds div 1000000,
  {Seconds, (Microseconds rem 1000000) * 1000};
datetime_to_seconds({Nanoseconds, nanosecond}) ->
  Seconds = Nanoseconds div 1000000000,
  {Seconds, Nanoseconds rem 1000000000};
datetime_to_seconds({Megaseconds, Seconds, Microseconds}) ->
  {Megaseconds * 1000000 + Seconds, Microseconds * 1000};
datetime_to_seconds(Datetime = {Date, Time}) when
    is_tuple(Date) andalso is_tuple(Time) ->
  % We would like to use calendar:datetime_to_system_time/1 but it is not
  % exported.
  DaysFrom0ToEpoch = 719528,
  SecondsFrom0ToEpoch = DaysFrom0ToEpoch * 86400,
  S = calendar:datetime_to_gregorian_seconds(Datetime) - SecondsFrom0ToEpoch,
  {S, 0}.
