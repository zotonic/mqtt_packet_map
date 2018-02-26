%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell

%% @doc MQTT encoder/decoder shared definitions
%%      See: http://docs.oasis-open.org/mqtt/mqtt/v5.0/cs01/mqtt-v5.0-cs01.html

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

% Control packet types
% http://docs.oasis-open.org/mqtt/mqtt/v5.0/cs01/mqtt-v5.0-cs01.html#_Toc502667245
-define(CONNECT,      1).
-define(CONNACK,      2).
-define(PUBLISH,      3).
-define(PUBACK,       4).
-define(PUBREC,       5).
-define(PUBREL,       6).
-define(PUBCOMP,      7).
-define(SUBSCRIBE,    8).
-define(SUBACK,       9).
-define(UNSUBSCRIBE, 10).
-define(UNSUBACK,    11).
-define(PINGREQ,     12).
-define(PINGRESP,    13).
-define(DISCONNECT,  14).
-define(AUTH,        15).

% The two MQTT protocol names
-define(PROTOCOL_NAME_3, <<"MQIsdp">>).
-define(PROTOCOL_NAME,   <<"MQTT">>).

% MQTT protocol levels
-define(MQTTv3,   3).
-define(MQTTv311, 4).
-define(MQTTv5,   5).

% Maximum size of the variable part of a packet (4 times 7 bits = 28 bits)
-define(MAX_PACKET_SIZE, 16#fffffff).

% Maximum value that can be encoded as a variable byte integer
-define(MAX_VARINT, 16#7fffffff).