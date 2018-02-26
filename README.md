[![Build Status](https://travis-ci.org/zotonic/mqtt_packet_map.svg?branch=master)](https://travis-ci.org/zotonic/mqtt_packet_map)


MQTT Packet Encoder and Decoder
===============================

Encoder and decoder for MQTT v5 and earlier.

Maps are used for the representation of all MQTT messages.
There are two functions:

    1. `mqtt_packet_map:encode/1`
    2. `mqtt_packet_map:decode/1`

Both have a variant where the MQTT version (protocol level) is passed.
This defaults to 5, valid values are 3, 4 (v3.1.1) and 5.

Example usage:

``erlang
% Decode an incoming binary, return the message
{ok, {Msg, RestBin}} =
case mqtt_packet_map:decode(Bin) of
    {ok, {Msg, RestBin}} ->
        % Decoded a packet, RestBin contains the
        % remaining data for the next packet(s).
        ...;
    {error, incomplete_packet} -> ...;
        % Packet is too short, fetch more data first
        ...;
    {error, malformed_header} ->
        % Illegal package, close the connection
        ...;
    {error, unknown_protocol} ->
        % Only for connect messages
        ...
end.

% Encode a message
{ok, Bin} = mqtt_packet_map:encode(Msg).

% Encode a message as MQTT v3.1.1 (protocol level 4)
{ok, Bin} = mqtt_packet_map:encode(4, Msg).
``

