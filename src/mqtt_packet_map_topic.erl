%% @doc MQTT packet encoder

%% Copyright 2018 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mqtt_packet_map_topic).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    validate_topic/1,
    validate_topic_publish/1,
    is_valid_topic/1,
    normalize_topic/1,
    flatten_topic/1,
    is_wildcard_topic/1
    ]).

%% @doc Validate a topic, return the normalized topic if it is a valid topic or topic filter.
-spec validate_topic( mqtt_packet_map:mqtt_topic() ) -> {ok, mqtt_packet_map:mqtt_topic()} | {error, invalid_topic}.
validate_topic(T) ->
    T1 = normalize_topic(T),
    case is_valid_topic(T1) of
        true -> {ok, T1};
        false -> {error, invalid_topic}
    end.

%% @doc Validate a topic, return the normalized topic. Must not contain any wild cards.
-spec validate_topic_publish( mqtt_packet_map:mqtt_topic() ) -> {ok, mqtt_packet_map:mqtt_topic()} | {error, invalid_topic}.
validate_topic_publish(T) ->
    case validate_topic(T) of
        {ok, T1} ->
            case is_wildcard_topic(T1) of
                true -> {error, invalid_topic};
                false -> {ok, T1}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Check if a topic is valid, the topic must have been normalized.
%%      All topic characters must be utf-8 and topic levels shouldn't contain + and # characters.
-spec is_valid_topic( list() ) -> boolean().
is_valid_topic([]) ->
    false;
is_valid_topic(L) when is_list(L) ->
    is_valid_topic_1(L).

is_valid_topic_1([]) ->
    true;
is_valid_topic_1([ '#' ]) ->
    true;
is_valid_topic_1([ '#' | _ ]) ->
    false;
is_valid_topic_1([ H | T ]) ->
    case is_valid_topic_part(H) of
        true -> is_valid_topic_1(T);
        false -> false
    end.

is_valid_topic_part('#') -> true;
is_valid_topic_part('+') -> true;
is_valid_topic_part(N) when is_integer(N) -> true;
is_valid_topic_part(B) when is_binary(B) -> is_valid_topic_part_chars(B).

is_valid_topic_part_chars(<<>>) -> true;
is_valid_topic_part_chars(<<$+, _/binary>>) -> false;
is_valid_topic_part_chars(<<$#, _/binary>>) -> false;
is_valid_topic_part_chars(<<$/, _/binary>>) -> false;
is_valid_topic_part_chars(<<0, _/binary>>) -> false;
is_valid_topic_part_chars(<<_/utf8, Rest/binary>>) -> is_valid_topic_part_chars(Rest);
is_valid_topic_part_chars(_) -> false.


%% @doc Normalize a topic to a list. Wildcards are replace by the atoms '+' and '#' (as used by the router).
-spec normalize_topic( mqtt_packet_map:mqtt_topic() ) -> mqtt_packet_map:mqtt_topic().
normalize_topic(<<>>) ->
    [];
normalize_topic(B) when is_binary(B) ->
    normalize_topic( binary:split(B, <<"/">>, [global]) );
normalize_topic(L) when is_list(L) ->
    lists:map(fun normalize_topic_part/1, L).

normalize_topic_part('+') -> '+';
normalize_topic_part('#') -> '#';
normalize_topic_part(<<"+">>) -> '+';
normalize_topic_part(<<"#">>) -> '#';
normalize_topic_part(T) when is_integer(T) -> integer_to_binary(T);
normalize_topic_part(T) when is_binary(T) -> T;
normalize_topic_part(T) -> z_convert:to_binary(T).

%% @doc Recombine a normalized topic to a single binary string.
-spec flatten_topic( mqtt_packet_map:mqtt_topic() ) -> binary().
flatten_topic(B) when is_binary(B) ->
    B;
flatten_topic([]) ->
    <<>>;
flatten_topic([ H | T ]) ->
    flatten_topic_list(T, to_binary(H)).

flatten_topic_list([], Acc) ->
    Acc;
flatten_topic_list([ H  | T ], Acc) ->
    H1 = to_binary(H),
    flatten_topic_list(T, <<Acc/binary, $/, H1/binary>>).

-spec is_wildcard_topic( list() ) -> boolean().
is_wildcard_topic(L) ->
    lists:any(fun is_wildcard/1, L).

is_wildcard('+') -> true;
is_wildcard('#') -> true;
is_wildcard(_) -> false.

to_binary(B) when is_binary(B) -> B;
to_binary('+') -> <<"+">>;
to_binary('#') -> <<"#">>;
to_binary(N) -> z_convert:to_binary(N).
