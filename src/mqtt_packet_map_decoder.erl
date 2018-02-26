%% @doc MQTT packet decoder

%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(mqtt_packet_map_decoder).

-author('Marc Worrell <marc@worrell.nl>').
-author('Feng Lee <feng@emqtt.io>').

-export([
    decode/2
]).

% testing
-export([
    parse_varint/1
    ]).

-include("mqtt_packet_map_defs.hrl").

-type decode_return() :: {ok, {mqtt_encoder:mqtt_packet(), binary()}} | {error, mqtt_encoder:decode_error()}.

%% @doc Decode an incoming MQTT packet, returns a decoded packet or an error.
-spec decode( pmqtt_encoder:mqtt_version(), binary() ) -> decode_return().
decode(MQTTVersion, <<Fixed:1/binary, 0:1, DataSize:7, VarData/binary>>) ->
    parse(MQTTVersion, DataSize, Fixed, VarData);
decode(MQTTVersion, <<Fixed:1/binary, 1:1, L1:7, 0:1, L2:7, VarData/binary>>) ->
    parse(MQTTVersion, L1 + (L2 bsl 7), Fixed, VarData);
decode(MQTTVersion, <<Fixed:1/binary, 1:1, L1:7, 1:1, L2:7, 0:1, L3:7, VarData/binary>>) ->
    parse(MQTTVersion, L1 + (L2 bsl 7) + (L3 bsl 14), Fixed, VarData);
decode(MQTTVersion, <<Fixed:1/binary, 1:1, L1:7, 1:1, L2:7, 1:1, L3:7, 0:1, L4:7, VarData/binary>>) ->
    parse(MQTTVersion, L1 + (L2 bsl 7) + (L3 bsl 14) + (L4 bsl 21), Fixed, VarData);
decode(_MQTTVersion, <<_:8/binary, _/binary>>) ->
    {error, malformed_header};
decode(_MQTTVersion, B) when is_binary(B) ->
    {error, incomplete_packet}.

%% @doc Check if we have enough data to parse the whole control packet
-spec parse( mqtt_encoder:mqtt_version(), pos_integer(), binary(), binary() ) -> decode_return().
parse(MQTTVersion, VarSize, Fixed, VarData) when size(VarData) >= VarSize ->
    <<Var:VarSize/binary, Rest/binary>> = VarData,
    case variable(MQTTVersion, Fixed, Var) of
        {ok, Msg} ->
            {ok, {Msg, Rest}};
        {error, _} = Error ->
            Error
    end;
parse(_MQTTVersion, _VarSize, _Fixed, _VarData) ->
    {error, incomplete_packet}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parse control packets %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parse the control packet
-spec variable( mqtt_encoder:mqtt_version(), Fixed::binary(), Variable::binary() ) ->
        {ok, mqtt_encoder:mqtt_packet()} | {error, mqtt_encoder:decode_error()}.
variable(_MQTTVersion,
         <<?CONNECT:4, 0:4>>,
         <<ProtocolNameLen:16/big, ProtocolName:ProtocolNameLen/binary,
           ProtocolLevel:8,
           UserNameFlag:1, PasswordFlag:1, WillRetain:1, WillQos:2, WillFlag:1, CleanStart:1, 0:1,
           KeepAlive:16/big,
           PropsRest/binary>>) ->
    case protocol_name_level(ProtocolName, ProtocolLevel) of
        {true, ProtoVersion} ->
            {Properties, Rest} = case ProtoVersion of
                ?MQTTv5 -> parse_properties(PropsRest);
                _ -> {#{}, PropsRest}
            end,
            {ClientId, Rest1} = parse_utf(Rest),
            {WillProperties, Rest2} = case ProtoVersion of
                ?MQTTv5 when WillFlag =:= 1 -> parse_properties(Rest1);
                _ -> {#{}, Rest1}
            end,
            {WillTopic,   Rest3} = parse_utf(Rest2, WillFlag),
            {WillPayload, Rest4} = parse_msg(Rest3, WillFlag),
            {Username,    Rest5} = parse_utf(Rest4, UserNameFlag),
            {Password,    <<>>}  = parse_utf(Rest5, PasswordFlag),
            {ok, #{
                type => 'connect',
                protocol_name => ProtocolName,
                protocol_version => ProtoVersion,
                client_id => ClientId,
                clean_start => bool(CleanStart),
                keep_alive => KeepAlive,
                properties => Properties,
                username => Username,
                password => Password,
                will_flag => bool(WillFlag),
                will_retain => bool(WillRetain),
                will_qos => qos(WillQos),
                will_properties => WillProperties,
                will_topic => split_topic(WillTopic),
                will_payload => WillPayload
            }};
        false ->
            {error, unknown_protocol}
    end;
variable(_MQTTVersion, <<?CONNECT:4, 0:4>>, <<L:16/big, _PMagic:L/binary, _/binary>>) ->
    {error, unknown_protocol};
variable(MQTTVersion, <<?CONNACK:4, 0:4>>, <<0:7, SessionPresent:1, ConnectReason:8, Rest/binary>>) ->
    {Properties, <<>>} = case MQTTVersion of
        ?MQTTv5 -> parse_properties(Rest);
        _ -> {#{}, Rest}
    end,
    {ok, #{
        type => 'connack',
        session_present => bool(SessionPresent),
        reason_code => ConnectReason,
        properties => Properties
    }};
variable(MQTTVersion, <<?PUBLISH:4, Dup:1, QoS:2, Retain:1>>, <<TopicLen:16/big, Topic:TopicLen/binary, Rest/binary>>) ->
    {PacketId, Rest2} = case QoS of
        0 ->
            {undefined, Rest};
        _ ->
            <<PId:16/big, R/binary>> = Rest,
            {PId, R}
    end,
    {Properties, Payload} = case MQTTVersion of
        ?MQTTv5 -> parse_properties(Rest2);
        _ -> {#{}, Rest2}
    end,
    {ok, #{
        type => 'publish',
        dup => bool(Dup),
        qos => qos(QoS),
        retain => bool(Retain),
        topic => split_topic(Topic),
        packet_id => PacketId,
        properties => Properties,
        payload => Payload
    }};
variable(MQTTVersion, <<P:4, 0:4>>, <<PacketId:16/big, Rest/binary>>)
    when P =:= ?PUBACK; P =:= ?PUBREC; P =:= ?PUBREL; P =:= ?PUBCOMP ->
    {ReasonCode, Properties} = case MQTTVersion of
        ?MQTTv5 ->
            <<RC:8, Rest1/binary>> = Rest,
            {Ps, <<>>} = parse_properties(Rest1),
            {RC, Ps};
        _ when Rest =:= <<>> ->
            {undefined, #{}}
    end,
    {ok, #{
        type => case P of
                    ?PUBACK -> 'puback';
                    ?PUBREC -> 'pubrec';
                    ?PUBREL -> 'pubrel';
                    ?PUBCOMP -> 'pubcomp'
                end,
        packet_id => PacketId,
        reason_code => ReasonCode,
        properties => Properties
    }};
variable(MQTTVersion, <<?SUBSCRIBE:4, 0:2, 1:1, 0:1>>, <<PacketId:16/big, Rest/binary>>) ->
    {Properties, Rest1} = case MQTTVersion of
        ?MQTTv5 -> parse_properties(Rest);
        _ -> {#{}, Rest}
    end,
    Topics = parse_subscribe_topics(Rest1, []),
    {ok, #{
        type => 'subscribe',
        packet_id => PacketId,
        topics => Topics,
        properties => Properties
    }};
variable(MQTTVersion, <<?SUBACK:4, 0:4>>, <<PacketId:16/big, Rest/binary>>) ->
    {Properties, Rest1} = case MQTTVersion of
        ?MQTTv5 -> parse_properties(Rest);
        _ -> {#{}, Rest}
    end,
    Acks = parse_acks(Rest1, []),
    {ok, #{
        type => 'suback',
        packet_id => PacketId,
        properties => Properties,
        acks => Acks
    }};
variable(MQTTVersion, <<?UNSUBSCRIBE:4, 0:2, 1:1, 0:1>>, <<PacketId:16/big, Rest/binary>>) ->
    {Properties, Rest1} = case MQTTVersion of
        ?MQTTv5 -> parse_properties(Rest);
        _ -> {#{}, Rest}
    end,
    Topics = parse_unsubscribe_topics(Rest1, []),
    {ok, #{
        type => 'unsubscribe',
        packet_id => PacketId,
        properties => Properties,
        topics => Topics
    }};
variable(MQTTVersion, <<?UNSUBACK:4, 0:4>>, <<PacketId:16/big, Rest/binary>>) ->
    {Properties, Rest1} = case MQTTVersion of
        ?MQTTv5 -> parse_properties(Rest);
        _ -> {#{}, Rest}
    end,
    Acks = parse_unacks(Rest1, []),
    {ok, #{
        type => 'unsuback',
        packet_id => PacketId,
        properties => Properties,
        acks => Acks
    }};
variable(_MQTTVersion, <<?PINGREQ:4, 0:4>>, <<>>) ->
    {ok, #{
        type => 'pingreq'
    }};
variable(_MQTTVersion, <<?PINGRESP:4, 0:4>>, <<>>) ->
    {ok, #{
        type => 'pingresp'
    }};
variable(_MQTTVersion, <<?DISCONNECT:4, 0:4>>, <<>>) ->
    {ok, #{
        type => 'disconnect',
        reason_code => 0,
        properties => #{}
    }};
variable(MQTTVersion, <<?DISCONNECT:4, 0:4>>, <<Reason:8, Rest/binary>>) ->
    {Properties, <<>>} = case MQTTVersion of
        ?MQTTv5 -> parse_properties(Rest);
        _ -> {#{}, Rest}
    end,
    {ok, #{
        type => 'disconnect',
        reason_code => Reason,
        properties => Properties
    }};
variable(?MQTTv5, <<?AUTH:4, 0:4>>, <<Reason:8, Rest/binary>>) ->
    {Properties, <<>>} = parse_properties(Rest),
    {ok, #{
        type => 'auth',
        reason_code => Reason,
        properties => Properties
    }};
variable(_MQTTVersion, _Fixed, _Var) ->
    {error, invalid_packet}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Support functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

protocol_name_level(?PROTOCOL_NAME_3, ?MQTTv3)   -> {true, ?MQTTv3};
protocol_name_level(?PROTOCOL_NAME,   ?MQTTv311) -> {true, ?MQTTv311};
protocol_name_level(?PROTOCOL_NAME,   ?MQTTv5)   -> {true, ?MQTTv5};
% Mosquitto bridge: 0x83, 0x84
protocol_name_level(?PROTOCOL_NAME_3, 16#83) -> {true, ?MQTTv3};
protocol_name_level(?PROTOCOL_NAME,   16#84) -> {true, ?MQTTv311};
protocol_name_level(?PROTOCOL_NAME,   16#85) -> {true, ?MQTTv5};
protocol_name_level(_, _) -> false.



parse_subscribe_topics(<<>>, Topics) ->
    lists:reverse(Topics);
parse_subscribe_topics(Bin, Topics) ->
    {Name, OptRest} = parse_utf(Bin),
    <<0:2, RH:2, RAP:1, NL:1, QoS:2, Rest/binary>> = OptRest,
    T = #{
        topic => split_topic(Name),
        retain_handling => RH,
        retain_as_published => bool(RAP),
        no_local => bool(NL),
        qos => qos(QoS)
    },
    parse_subscribe_topics(Rest, [ T | Topics ]).

parse_unsubscribe_topics(<<>>, Topics) ->
    lists:reverse(Topics);
parse_unsubscribe_topics(Bin, Topics) ->
    {Name, Rest} = parse_utf(Bin),
    parse_unsubscribe_topics(Rest, [ split_topic(Name) | Topics ]).

parse_acks(<<>>, Acks) ->
    lists:reverse(Acks);
parse_acks(<<0:6, QoS:2, Rest/binary>>, Acks) ->
    parse_acks(Rest, [ {ok, qos(QoS)} | Acks]);
parse_acks(<<Reason:8, Rest/binary>>, Acks) when Reason >= 16#80 ->
    parse_acks(Rest, [ {error, Reason} | Acks]).

parse_unacks(<<>>, Acks) ->
    lists:reverse(Acks);
parse_unacks(<<0, Rest/binary>>, Acks) ->
    parse_unacks(Rest, [ {ok, found} | Acks]);
parse_unacks(<<17, Rest/binary>>, Acks) ->
    parse_unacks(Rest, [ {ok, notfound} | Acks]);
parse_unacks(<<Reason:8, Rest/binary>>, Acks) when Reason >= 16#80 ->
    parse_unacks(Rest, [ {error, Reason} | Acks]).


%%%%%
%% Following functions are adapted from emqttd
%% emqttd is: (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
%%%%%

parse_properties(<<>>) ->
    {#{}, <<>>};
parse_properties(Bin) ->
    {Len, Bin1} = parse_varint(Bin),
    <<PropBin:Len/binary, Rest/binary>> = Bin1,
    {parse_property(PropBin, #{}), Rest}.

parse_property(<<>>, Props) ->
    Props;
parse_property(<<16#01, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'payload_format_indicator' => bool(Val) });
parse_property(<<16#02, Val:32/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'message_expiry_interval' => Val });
parse_property(<<16#03, Bin/binary>>, Props) ->
    {Val, Rest} = parse_utf(Bin),
    parse_property(Rest, Props#{ 'content_type' => Val });
parse_property(<<16#08, Bin/binary>>, Props) ->
    {Val, Rest} = parse_utf(Bin),
    parse_property(Rest, Props#{ 'response_topic' => split_topic(Val) });
parse_property(<<16#09, Bin/binary>>, Props) ->
    {Val, Rest} = parse_bin(Bin),
    parse_property(Rest, Props#{ 'correlation_data' => Val });
parse_property(<<16#0B, Bin/binary>>, Props) ->
    {Val, Rest} = parse_varint(Bin),
    parse_property(Rest, Props#{ 'subscription_identifier' => Val });
parse_property(<<16#11, Val:32/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'session_expiry_interval' => Val });
parse_property(<<16#12, Bin/binary>>, Props) ->
    {Val, Rest} = parse_utf(Bin),
    parse_property(Rest, Props#{ 'assigned_client_identifier' => Val });
parse_property(<<16#13, Val:16/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'server_keep_alive' => Val });
parse_property(<<16#15, Bin/binary>>, Props) ->
    {Val, Rest} = parse_utf(Bin),
    parse_property(Rest, Props#{ 'authentication_method' => Val });
parse_property(<<16#16, Bin/binary>>, Props) ->
    {Val, Rest} = parse_bin(Bin),
    parse_property(Rest, Props#{ 'authentication_data' => Val });
parse_property(<<16#17, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{'request_problem_information' => bool(Val) });
parse_property(<<16#18, Val:32/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'will_delay_interval' => Val });
parse_property(<<16#19, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'request_response_information' => bool(Val)});
parse_property(<<16#1A, Bin/binary>>, Props) ->
    {Val, Rest} = parse_utf(Bin),
    parse_property(Rest, Props#{ 'response_information' => Val });
parse_property(<<16#1C, Bin/binary>>, Props) ->
    {Val, Rest} = parse_utf(Bin),
    parse_property(Rest, Props#{ 'server_reference' => Val });
parse_property(<<16#1F, Bin/binary>>, Props) ->
    {Val, Rest} = parse_utf(Bin),
    parse_property(Rest, Props#{ 'reason_string' => Val });
parse_property(<<16#21, Val:16/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'receive_maximum' => Val});
parse_property(<<16#22, Val:16/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'topic_alias_maximum' => Val});
parse_property(<<16#23, Val:16/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'topic_alias' => Val });
parse_property(<<16#24, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'maximum_qos' => Val });
parse_property(<<16#25, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'retain_available' => bool(Val) });
parse_property(<<16#26, Bin/binary>>, Props) ->
    % User properties use binary keys in the properties map
    {{Key, Val}, Rest} = parse_utf_pair(Bin),
    parse_property(Rest, Props#{ Key => Val });
parse_property(<<16#27, Val:32/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'maximum_packet_size' => Val });
parse_property(<<16#28, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'wildcard_subscription_available' => bool(Val) });
parse_property(<<16#29, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'subscription_identifier_available' => bool(Val) });
parse_property(<<16#2A, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'shared_subscription_available' => bool(Val) }).



parse_varint(B) ->
    parse_varint(B, 0, 0).

parse_varint(<<0:1, I:7, Rest/binary>>, Shift, Value) ->
    {Value + (I bsl Shift), Rest};
parse_varint(<<1:1, I:7, Rest/binary>>, Shift, Value) ->
    parse_varint(Rest, Shift + 7, Value + (I bsl Shift)).

split_topic(undefined) ->
    undefined;
split_topic(Topic) ->
    binary:split(Topic, <<"/">>, [global]).

parse_utf_pair(Bin) ->
    {Key, Bin1} = parse_utf(Bin),
    {Val, Rest} = parse_utf(Bin1),
    {{Key, Val}, Rest}.

parse_utf(Bin, 0) ->
    {undefined, Bin};
parse_utf(Bin, 1) ->
    parse_utf(Bin).

parse_utf(<<Len:16/big, Str:Len/binary, Rest/binary>>) ->
    {Str, Rest}.

parse_msg(Bin, 0) ->
    {undefined, Bin};
parse_msg(<<Len:16/big, Msg:Len/binary, Rest/binary>>, _) ->
    {Msg, Rest}.

parse_bin(<<Len:16/big, Bin:Len/binary, Rest/binary>>) ->
    {Bin, Rest}.

bool(0) -> false;
bool(1) -> true.

qos(0) -> 0;
qos(1) -> 1;
qos(2) -> 2.
