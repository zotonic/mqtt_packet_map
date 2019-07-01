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
    decode/2
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

-export_type([
    mqtt_version/0,
    mqtt_packet/0,
    mqtt_topic/0,
    decode_error/0
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

