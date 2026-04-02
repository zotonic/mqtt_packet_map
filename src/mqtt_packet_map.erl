%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell

%% @doc MQTT packet encoder/decoder

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

-module(mqtt_packet_map).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    encode/1,
    encode/2,
    decode/1,
    decode/2,
    check_packet_size/2,
    packet_size/1
]).

-include("mqtt_packet_map_defs.hrl").

-type mqtt_version() :: undefined | ?MQTTv3 | ?MQTTv311 | ?MQTTv5.
-type mqtt_packet() :: map().
-type mqtt_topic() :: list(binary() | integer() | '+' | '#') | binary().
-type decode_error() :: incomplete_packet
                      | malformed_header
                      | unknown_protocol
                      | invalid_packet
                      | invalid_topic.

-type packet_size_error() :: malformed_packet | packet_too_large.

-export_type([
    mqtt_version/0,
    mqtt_packet/0,
    mqtt_topic/0,
    decode_error/0,
    packet_size_error/0
]).

%% @doc Encode a MQTT message to a binary.
-spec encode( mqtt_packet() ) -> {ok, binary()} | {error, term()}.
encode(Msg) ->
    mqtt_packet_map_encoder:encode(?MQTTv5, Msg).

%% @doc Encode a MQTT message to a binary.
-spec encode( mqtt_version(), mqtt_packet() ) -> {ok, binary()} | {error, term()}.
encode(MQTTVersion, Msg) ->
    mqtt_packet_map_encoder:encode(MQTTVersion, Msg).


%% @@doc Decode an incoming MQTT message, returns a decoded packet or an error.
-spec decode( binary() ) -> {ok, {mqtt_packet(), binary()}} | {error, decode_error()}.
decode(Data) ->
    mqtt_packet_map_decoder:decode(?MQTTv5, Data).

%% @@doc Decode an incoming MQTT message, returns a decoded packet or an error.
-spec decode( mqtt_version(), binary() ) -> {ok, {mqtt_packet(), binary()}} | {error, decode_error()}.
decode(MQTTVersion, Data) ->
    mqtt_packet_map_decoder:decode(MQTTVersion, Data).

%% @doc Check if a binary contains a complete packet within the maximum packet size.
-spec check_packet_size(binary(), pos_integer() | undefined) ->
    ok | incomplete | {error, packet_size_error()}.
check_packet_size(Data, undefined) ->
    case packet_size(Data) of
        {ok, _PacketSize} ->
            ok;
        {error, _} = Error ->
            Error;
        incomplete ->
            incomplete
    end;
check_packet_size(Data, MaxPacketSize) ->
    case packet_size(Data) of
        {ok, PacketSize} when PacketSize =< MaxPacketSize ->
            ok;
        {ok, _PacketSize} ->
            {error, packet_too_large};
        {error, _} = Error ->
            Error;
        incomplete ->
            incomplete
    end.

%% @doc Return the full packet size, including the fixed header, when available.
-spec packet_size(binary()) -> {ok, non_neg_integer()} | incomplete | {error, malformed_packet}.
packet_size(<<_PacketType:8, Rest/binary>>) ->
    case remaining_length(Rest, 0, 1, 0) of
        {ok, RemainingLength, LengthBytes} ->
            {ok, 1 + LengthBytes + RemainingLength};
        {error, _} = Error ->
            Error;
        incomplete ->
            incomplete
    end;
packet_size(<<>>) ->
    incomplete.

-spec remaining_length(binary(), non_neg_integer(), pos_integer(), non_neg_integer()) ->
    {ok, non_neg_integer(), pos_integer()} | incomplete | {error, malformed_packet}.
remaining_length(_Rest, _Value, _Multiplier, Count) when Count >= 4 ->
    {error, malformed_packet};
remaining_length(<<>>, _Value, _Multiplier, _Count) ->
    incomplete;
remaining_length(<<Byte:8, Rest/binary>>, Value, Multiplier, Count) ->
    Value1 = Value + ((Byte band 16#7f) * Multiplier),
    case Byte band 16#80 of
        16#80 ->
            remaining_length(Rest, Value1, Multiplier * 128, Count + 1);
        0 when Value1 > 268435455 ->
            {error, malformed_packet};
        0 ->
            {ok, Value1, Count + 1}
    end.
