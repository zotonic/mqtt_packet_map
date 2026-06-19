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
-include("mqtt_packet_map.hrl").

-type decode_return() :: {ok, {mqtt_packet_map:mqtt_packet(), binary()}} | {error, mqtt_packet_map:decode_error()}.

-define(MAX_VARINT_BYTES, 4).

%% @doc Decode an incoming MQTT packet, returns a decoded packet or an error.
-spec decode( mqtt_packet_map:mqtt_version(), binary() ) -> decode_return().
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
-spec parse( mqtt_packet_map:mqtt_version(), pos_integer(), binary(), binary() ) -> decode_return().
parse(MQTTVersion, VarSize, Fixed, VarData) when size(VarData) >= VarSize ->
    <<Var:VarSize/binary, Rest/binary>> = VarData,
    case variable(MQTTVersion, Fixed, Var) of
        {ok, Msg} ->
            {ok, {Msg, Rest}};
        {error, incomplete_packet} ->
            % Data was exhausted inside variable/3, but we had the full packet
            % according to the fixed header. This means the packet is malformed.
            {error, malformed_packet};
        {error, _} = Error ->
            Error
    end;
parse(_MQTTVersion, _VarSize, _Fixed, _VarData) ->
    {error, incomplete_packet}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parse control packets %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parse the control packet
-spec variable( mqtt_packet_map:mqtt_version(), Fixed::binary(), Variable::binary() ) ->
        {ok, mqtt_packet_map:mqtt_packet()} | {error, mqtt_packet_map:decode_error()}.
variable(_MQTTVersion,
         <<?CONNECT:4, 0:4>>,
         <<ProtocolNameLen:16/big, ProtocolName:ProtocolNameLen/binary,
           ProtocolLevel:8,
           UserNameFlag:1, PasswordFlag:1, WillRetain:1, WillQos:2, WillFlag:1, CleanStart:1, 0:1,
           KeepAlive:16/big,
           PropsRest/binary>>) ->
    case protocol_name_level(ProtocolName, ProtocolLevel) of
        {true, ProtoVersion} ->
            case {parse_bool(CleanStart), parse_bool(WillFlag), parse_bool(WillRetain), parse_qos(WillQos)} of
                {{ok, CleanStart1}, {ok, WillFlag1}, {ok, WillRetain1}, {ok, WillQos1}} ->
                    Connect = #{
                                type => 'connect',
                                protocol_name => ProtocolName,
                                protocol_version => ProtoVersion,
                                clean_start => CleanStart1,
                                keep_alive => KeepAlive,
                                will_flag => WillFlag1,
                                will_retain => WillRetain1,
                                will_qos => WillQos1
                               },
                    decode_connect_properties(PropsRest, Connect, UserNameFlag, PasswordFlag);
                _ ->
                    {error, malformed_packet}
            end;
        false ->
            {error, unknown_protocol}
    end;
variable(_MQTTVersion, <<?CONNECT:4, 0:4>>, <<L:16/big, _PMagic:L/binary, _/binary>>) ->
    {error, unknown_protocol};
variable(MQTTVersion, <<?CONNACK:4, 0:4>>, <<0:7, SessionPresent:1, ConnectReason:8, Rest/binary>>) ->
    case {parse_properties(MQTTVersion, Rest), parse_bool(SessionPresent)} of
        {{ok, {Properties, <<>>}}, {ok, SessionPresent1}} ->
            {ok, #{
                   type => 'connack',
                   session_present => SessionPresent1,
                   reason_code => ConnectReason,
                   properties => Properties
                  }};
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error;
        {_, _} -> {error, malformed_packet}
    end;
variable(MQTTVersion, <<?PUBLISH:4, Dup:1, QoS:2, Retain:1>>, <<TopicLen:16/big, Topic:TopicLen/binary, Rest/binary>>) ->
    {PacketId, Rest2} = case QoS of
        0 ->
            {undefined, Rest};
        _ ->
            case Rest of
                <<PId:16/big, R/binary>> -> {PId, R};
                _ -> {undefined, error}
            end
    end,
    case PacketId of
        undefined when Rest2 =:= error -> {error, incomplete_packet};
        _ ->
            case parse_properties(MQTTVersion, Rest2) of
                {ok, {Properties, Payload}} ->
                    case split_topic(Topic) of
                        {ok, TopicValidated} ->
                            case mqtt_packet_map_topic:is_wildcard_topic(TopicValidated) of
                                false ->
                                    case {parse_bool(Dup), parse_qos(QoS), parse_bool(Retain)} of
                                        {{ok, Dup1}, {ok, QoS1}, {ok, Retain1}} ->
                                            {ok, #{
                                                   type => 'publish',
                                                   dup => Dup1,
                                                   qos => QoS1,
                                                   retain => Retain1,
                                                   topic => TopicValidated,
                                                   packet_id => PacketId,
                                                   properties => Properties,
                                                   payload => Payload
                                                  }};
                                        _ ->
                                            {error, malformed_packet}
                                    end;
                                true ->
                                    {error, invalid_topic}
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = ErrP -> ErrP
            end
    end;
variable(MQTTVersion, <<P:4, 0:4>>, <<PacketId:16/big, Rest/binary>>)
    when P =:= ?PUBACK; P =:= ?PUBREC; P =:= ?PUBCOMP ->
    Res = case MQTTVersion of
        ?MQTTv5 when Rest =:= <<>> ->
            {ok, {?MQTT_RC_SUCCESS, #{}}};
        ?MQTTv5 ->
            case Rest of
                <<RC:8, Rest1/binary>> ->
                    case parse_properties(Rest1) of
                        {ok, {Ps, <<>>}} -> {ok, {RC, Ps}};
                        {error, _} = ErrP -> ErrP
                    end;
                _ -> {error, incomplete_packet}
            end;
        _ when Rest =:= <<>> ->
            {ok, {?MQTT_RC_SUCCESS, #{}}}
    end,
    case Res of
        {ok, {ReasonCode, Properties}} ->
            {ok, #{
                type => case P of
                            ?PUBACK -> 'puback';
                            ?PUBREC -> 'pubrec';
                            ?PUBCOMP -> 'pubcomp'
                        end,
                packet_id => PacketId,
                reason_code => ReasonCode,
                properties => Properties
            }};
        {error, _} = Err -> Err
    end;
variable(MQTTVersion, <<?PUBREL:4, 2:4>>, <<PacketId:16/big, Rest/binary>>) ->
    Res = case MQTTVersion of
        ?MQTTv5 when Rest =:= <<>> ->
            {ok, {?MQTT_RC_SUCCESS, #{}}};
        ?MQTTv5 ->
            case Rest of
                <<RC:8, Rest1/binary>> ->
                    case parse_properties(Rest1) of
                        {ok, {Ps, <<>>}} -> {ok, {RC, Ps}};
                        {error, _} = ErrP -> ErrP
                    end;
                _ -> {error, incomplete_packet}
            end;
        _ when Rest =:= <<>> ->
            {ok, {?MQTT_RC_SUCCESS, #{}}}
    end,
    case Res of
        {ok, {ReasonCode, Properties}} ->
            {ok, #{
                type => 'pubrel',
                packet_id => PacketId,
                reason_code => ReasonCode,
                properties => Properties
            }};
        {error, _} = Err -> Err
    end;
variable(MQTTVersion, <<?SUBSCRIBE:4, 0:2, 1:1, 0:1>>, <<PacketId:16/big, Rest/binary>>) ->
    case parse_properties(MQTTVersion, Rest) of
        {ok, {Properties, Rest1}} ->
            case parse_subscribe_topics(Rest1, []) of
                {ok, Topics} ->
                    {ok, #{
                        type => 'subscribe',
                        packet_id => PacketId,
                        topics => Topics,
                        properties => Properties
                    }};
                {error, _} = ErrT ->
                    ErrT
            end;
        {error, _} = ErrP -> ErrP
    end;
variable(MQTTVersion, <<?SUBACK:4, 0:4>>, <<PacketId:16/big, Rest/binary>>) ->
    case parse_properties(MQTTVersion, Rest) of
        {ok, {Properties, Rest1}} ->
            case parse_acks(Rest1, []) of
                {ok, Acks} ->
                    {ok, #{
                        type => 'suback',
                        packet_id => PacketId,
                        properties => Properties,
                        acks => Acks
                    }};
                {error, _} = ErrA ->
                    ErrA
            end;
        {error, _} = ErrP -> ErrP
    end;
variable(MQTTVersion, <<?UNSUBSCRIBE:4, 0:2, 1:1, 0:1>>, <<PacketId:16/big, Rest/binary>>) ->
    case parse_properties(MQTTVersion, Rest) of
        {ok, {Properties, Rest1}} ->
            case parse_unsubscribe_topics(Rest1, []) of
                {ok, Topics} ->
                    {ok, #{
                        type => 'unsubscribe',
                        packet_id => PacketId,
                        properties => Properties,
                        topics => Topics
                    }};
                {error, _} = ErrT ->
                    ErrT
            end;
        {error, _} = ErrP -> ErrP
    end;
variable(MQTTVersion, <<?UNSUBACK:4, 0:4>>, <<PacketId:16/big, Rest/binary>>) ->
    case parse_properties(MQTTVersion, Rest) of
        {ok, {Properties, Rest1}} ->
            case parse_unacks(Rest1, []) of
                {ok, Acks} ->
                    {ok, #{
                        type => 'unsuback',
                        packet_id => PacketId,
                        properties => Properties,
                        acks => Acks
                    }};
                {error, _} = ErrA ->
                    ErrA
            end;
        {error, _} = ErrP -> ErrP
    end;
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
    case parse_properties(MQTTVersion, Rest) of
        {ok, {Properties, <<>>}} ->
            {ok, #{
                type => 'disconnect',
                reason_code => Reason,
                properties => Properties
            }};
        {error, _} = ErrP -> ErrP
    end;
variable(?MQTTv5, <<?AUTH:4, 0:4>>, <<>>) ->
    {ok, #{
        type => 'auth',
        reason_code => 0,
        properties => #{}
    }};
variable(?MQTTv5 = MQTTVersion, <<?AUTH:4, 0:4>>, <<Reason:8, Rest/binary>>) ->
    case parse_properties(MQTTVersion, Rest) of
        {ok, {Properties, <<>>}} ->
            {ok, #{
                type => 'auth',
                reason_code => Reason,
                properties => Properties
            }};
        {error, _} = ErrP -> ErrP
    end;
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

decode_connect_properties(Bin, Connect, UserNameFlag, PasswordFlag) ->
    ProtoVersion = maps:get(protocol_version, Connect),
    case parse_properties(ProtoVersion, Bin) of
        {ok, {Properties, Rest}} ->
            decode_connect_client_id(Rest, Connect#{ properties => Properties }, UserNameFlag, PasswordFlag);
        {error, _} = Err -> Err
    end.

decode_connect_client_id(Bin, Connect, UserNameFlag, PasswordFlag) ->
    case parse_utf(Bin) of
        {ok, {ClientId, Rest}} ->
            decode_connect_will_properties(Rest, Connect#{ client_id => ClientId }, UserNameFlag, PasswordFlag);
        {error, _} = Err -> Err
    end.

decode_connect_will_properties(Bin, #{ will_flag := true, protocol_version := ProtoVersion } = Connect, UserNameFlag, PasswordFlag) ->
    case parse_properties(ProtoVersion, Bin) of
        {ok, {WillProperties, Rest}} ->
            decode_connect_will_topic(Rest, Connect#{ will_properties => WillProperties }, UserNameFlag, PasswordFlag);
        {error, _} = Err -> Err
    end;
decode_connect_will_properties(Bin, Connect, UserNameFlag, PasswordFlag) ->
    decode_connect_will_topic(Bin, Connect#{ will_properties => #{} }, UserNameFlag, PasswordFlag).

decode_connect_will_topic(Bin, #{ will_flag := true } = Connect, UserNameFlag, PasswordFlag) ->
    case parse_utf(Bin) of
        {ok, {WillTopic, Rest}} ->
            case split_topic(WillTopic) of
                {ok, WillTopicValidated} ->
                    decode_connect_will_msg(Rest, Connect#{ will_topic => WillTopicValidated }, UserNameFlag, PasswordFlag);
                {error, _} = Err -> Err
            end;
        {error, _} = Err -> Err
    end;
decode_connect_will_topic(Bin, Connect, UserNameFlag, PasswordFlag) ->
    decode_connect_will_msg(Bin, Connect#{ will_topic => undefined }, UserNameFlag, PasswordFlag).

decode_connect_will_msg(Bin, #{ will_flag := true } = Connect, UserNameFlag, PasswordFlag) ->
    case parse_bin(Bin) of
        {ok, {WillPayload, Rest}} ->
            decode_connect_username(Rest, Connect#{ will_payload => WillPayload }, UserNameFlag, PasswordFlag);
        {error, _} = Err -> Err
    end;
decode_connect_will_msg(Bin, Connect, UserNameFlag, PasswordFlag) ->
    decode_connect_username(Bin, Connect#{ will_payload => undefined }, UserNameFlag, PasswordFlag).

decode_connect_username(Bin, Connect, 1, PasswordFlag) ->
    case parse_utf(Bin) of
        {ok, {Username, Rest}} ->
            decode_connect_password(Rest, Connect#{ username => Username }, PasswordFlag);
        {error, _} = Err -> Err
    end;
decode_connect_username(Bin, Connect, 0, PasswordFlag) ->
    decode_connect_password(Bin, Connect#{ username => undefined }, PasswordFlag).

decode_connect_password(Bin, Connect, 1) ->
    case parse_utf(Bin) of
        {ok, {Password, <<>>}} ->
            {ok, Connect#{ password => Password }};
        {ok, {_Password, _Rest}} ->
            {error, malformed_packet};
        {error, _} = Err -> Err
    end;
decode_connect_password(<<>>, Connect, 0) ->
    {ok, Connect#{ password => undefined }};
decode_connect_password(_Bin, _Connect, 0) ->
    {error, malformed_packet}.


parse_subscribe_topics(<<>>, Topics) ->
    {ok, lists:reverse(Topics)};
parse_subscribe_topics(Bin, Topics) ->
    case parse_utf(Bin) of
        {ok, {Name, <<0:2, RH:2, RAP:1, NL:1, QoS:2, Rest/binary>>}} ->
            case split_topic(Name) of
                {ok, NameValidated} ->
                    case {parse_bool(RAP), parse_bool(NL), parse_qos(QoS)} of
                        {{ok, RAP1}, {ok, NL1}, {ok, QoS1}} ->
                            T = #{
                                  topic => NameValidated,
                                  retain_handling => RH,
                                  retain_as_published => RAP1,
                                  no_local => NL1,
                                  qos => QoS1
                                 },
                            parse_subscribe_topics(Rest, [ T | Topics ]);
                        _ ->
                            {error, malformed_packet}
                    end;
                {error, _} = ErrT ->
                    ErrT
            end;
        {ok, {_, <<>>}} -> {error, incomplete_packet};
        {ok, _} -> {error, malformed_packet};
        {error, _} = ErrUTF -> ErrUTF
    end.

parse_unsubscribe_topics(<<>>, Topics) ->
    {ok, lists:reverse(Topics)};
parse_unsubscribe_topics(Bin, Topics) ->
    case parse_utf(Bin) of
        {ok, {Name, Rest}} ->
            case split_topic(Name) of
                {ok, NameValidated} ->
                    parse_unsubscribe_topics(Rest, [ NameValidated | Topics ]);
                {error, _} = ErrT ->
                    ErrT
            end;
        {error, _} = ErrUTF -> ErrUTF
    end.


parse_acks(<<>>, Acks) ->
    {ok, lists:reverse(Acks)};
parse_acks(<<0:6, QoS:2, Rest/binary>>, Acks) ->
    case parse_qos(QoS) of
        {ok, QoS1} ->
            parse_acks(Rest, [ {ok, QoS1} | Acks]);
        {error, _} = Err ->
            Err
    end;
parse_acks(<<Reason:8, Rest/binary>>, Acks) ->
    parse_acks(Rest, [ {error, Reason} | Acks]);
parse_acks(_, _) ->
    {error, incomplete_packet}.

parse_unacks(<<>>, Acks) ->
    {ok, lists:reverse(Acks)};
parse_unacks(<<0, Rest/binary>>, Acks) ->
    parse_unacks(Rest, [ {ok, found} | Acks]);
parse_unacks(<<17, Rest/binary>>, Acks) ->
    parse_unacks(Rest, [ {ok, notfound} | Acks]);
parse_unacks(<<Reason:8, Rest/binary>>, Acks) ->
    parse_unacks(Rest, [ {error, Reason} | Acks]);
parse_unacks(_, _) ->
    {error, incomplete_packet}.


%%%%%
%% Following functions are adapted from emqttd
%% emqttd is: (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
%%%%%

parse_properties(?MQTTv5, PropsRest) ->
    parse_properties(PropsRest);
parse_properties(_ProtoVersion, PropsRest) ->
    {ok, {#{}, PropsRest}}.

parse_properties(<<>>) ->
    {ok, {#{}, <<>>}};
parse_properties(Bin) ->
    case parse_varint(Bin) of
        {ok, {Len, Bin1}} ->
            case Bin1 of
                <<PropBin:Len/binary, Rest/binary>> ->
                    case parse_property(PropBin, #{}) of
                        {ok, Props} ->
                            {ok, {Props, Rest}};
                        {error, _} = Error ->
                            Error
                    end;
                _ ->
                    {error, incomplete_packet}
            end;
        {error, _} = Error ->
            Error
    end.

parse_property(<<>>, Props) ->
    Props1 = case maps:get('subscription_identifier', Props, undefined) of
                 Vs when is_list(Vs) -> Props#{ 'subscription_identifier' => lists:reverse(Vs) };
                 _ -> Props
             end,
    {ok, Props1};
parse_property(<<16#01, Val:8, Rest/binary>>, Props) ->
    case parse_bool(Val) of
        {ok, PFI} ->
            parse_property(Rest, Props#{ 'payload_format_indicator' => PFI });
        {error, _} = Error ->
            Error
    end;
parse_property(<<16#02, Val:32/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'message_expiry_interval' => Val });
parse_property(<<16#03, Bin/binary>>, Props) ->
    case parse_utf(Bin) of
        {ok, {Val, Rest}} -> parse_property(Rest, Props#{ 'content_type' => Val });
        {error, _} = ErrUTF -> ErrUTF
    end;
parse_property(<<16#08, Bin/binary>>, Props) ->
    case parse_utf(Bin) of
        {ok, {Val, Rest}} ->
            case split_topic(Val) of
                {ok, TopicValidated} ->
                    parse_property(Rest, Props#{ 'response_topic' => TopicValidated });
                {error, _} = ErrT ->
                    ErrT
            end;
        {error, _} = ErrUTF -> ErrUTF
    end;
parse_property(<<16#09, Bin/binary>>, Props) ->
    case parse_bin(Bin) of
        {ok, {Val, Rest}} -> parse_property(Rest, Props#{ 'correlation_data' => Val });
        {error, _} = ErrBin -> ErrBin
    end;
parse_property(<<16#0B, Bin/binary>>, Props) ->
    case parse_varint(Bin) of
        {ok, {Val, Rest}} ->
            Props1 = case maps:get('subscription_identifier', Props, undefined) of
                         undefined ->
                             Props#{ 'subscription_identifier' => Val };
                         Vs when is_list(Vs) ->
                             Props#{ 'subscription_identifier' => [ Val | Vs ] };
                         V when is_integer(V) ->
                             Props#{ 'subscription_identifier' => [ Val, V ] }
                     end,
            parse_property(Rest, Props1);
        {error, _} = Error ->
            Error
    end;
parse_property(<<16#11, Val:32/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'session_expiry_interval' => Val });
parse_property(<<16#12, Bin/binary>>, Props) ->
    case parse_utf(Bin) of
        {ok, {Val, Rest}} -> parse_property(Rest, Props#{ 'assigned_client_identifier' => Val });
        {error, _} = ErrUTF -> ErrUTF
    end;
parse_property(<<16#13, Val:16/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'server_keep_alive' => Val });
parse_property(<<16#15, Bin/binary>>, Props) ->
    case parse_utf(Bin) of
        {ok, {Val, Rest}} -> parse_property(Rest, Props#{ 'authentication_method' => Val });
        {error, _} = ErrUTF -> ErrUTF
    end;
parse_property(<<16#16, Bin/binary>>, Props) ->
    case parse_bin(Bin) of
        {ok, {Val, Rest}} -> parse_property(Rest, Props#{ 'authentication_data' => Val });
        {error, _} = ErrBin -> ErrBin
    end;
parse_property(<<16#17, Val:8, Rest/binary>>, Props) ->
    case parse_bool(Val) of
        {ok, RPI} ->
            parse_property(Rest, Props#{'request_problem_information' => RPI });
        {error, _} = Error ->
            Error
    end;
parse_property(<<16#18, Val:32/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'will_delay_interval' => Val });
parse_property(<<16#19, Val:8, Rest/binary>>, Props) ->
    case parse_bool(Val) of
        {ok, RRI} ->
            parse_property(Rest, Props#{ 'request_response_information' => RRI });
        {error, _} = Error ->
            Error
    end;
parse_property(<<16#1A, Bin/binary>>, Props) ->
    case parse_utf(Bin) of
        {ok, {Val, Rest}} -> parse_property(Rest, Props#{ 'response_information' => Val });
        {error, _} = ErrUTF -> ErrUTF
    end;
parse_property(<<16#1C, Bin/binary>>, Props) ->
    case parse_utf(Bin) of
        {ok, {Val, Rest}} -> parse_property(Rest, Props#{ 'server_reference' => Val });
        {error, _} = ErrUTF -> ErrUTF
    end;
parse_property(<<16#1F, Bin/binary>>, Props) ->
    case parse_utf(Bin) of
        {ok, {Val, Rest}} -> parse_property(Rest, Props#{ 'reason_string' => Val });
        {error, _} = ErrUTF -> ErrUTF
    end;
parse_property(<<16#21, Val:16/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'receive_maximum' => Val});
parse_property(<<16#22, Val:16/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'topic_alias_maximum' => Val});
parse_property(<<16#23, Val:16/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'topic_alias' => Val });
parse_property(<<16#24, Val:8, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'maximum_qos' => Val });
parse_property(<<16#25, Val:8, Rest/binary>>, Props) ->
    case parse_bool(Val) of
        {ok, RA} ->
            parse_property(Rest, Props#{ 'retain_available' => RA });
        {error, _} = Error ->
            Error
    end;
parse_property(<<16#26, Bin/binary>>, Props) ->
    % User properties use binary keys in the properties map
    case parse_utf_pair(Bin) of
        {ok, {{Key, Val}, Rest}} -> parse_property(Rest, Props#{ Key => Val });
        {error, _} = ErrUTFP -> ErrUTFP
    end;
parse_property(<<16#27, Val:32/big, Rest/binary>>, Props) ->
    parse_property(Rest, Props#{ 'maximum_packet_size' => Val });
parse_property(<<16#28, Val:8, Rest/binary>>, Props) ->
    case parse_bool(Val) of
        {ok, WSA} ->
            parse_property(Rest, Props#{ 'wildcard_subscription_available' => WSA });
        {error, _} = Error ->
            Error
    end;
parse_property(<<16#29, Val:8, Rest/binary>>, Props) ->
    case parse_bool(Val) of
        {ok, SIA} ->
            parse_property(Rest, Props#{ 'subscription_identifier_available' => SIA });
        {error, _} = Error ->
            Error
    end;
parse_property(<<16#2A, Val:8, Rest/binary>>, Props) ->
    case parse_bool(Val) of
        {ok, SSA} ->
            parse_property(Rest, Props#{ 'shared_subscription_available' => SSA });
        {error, _} = Error ->
            Error
    end;
parse_property(<<ID:8, _/binary>>, _Props) ->
    case is_known_property(ID) of
        true -> {error, incomplete_packet};
        false -> {error, {unknown_property, ID}}
    end.

is_known_property(16#01) -> true;
is_known_property(16#02) -> true;
is_known_property(16#03) -> true;
is_known_property(16#08) -> true;
is_known_property(16#09) -> true;
is_known_property(16#0B) -> true;
is_known_property(16#11) -> true;
is_known_property(16#12) -> true;
is_known_property(16#13) -> true;
is_known_property(16#15) -> true;
is_known_property(16#16) -> true;
is_known_property(16#17) -> true;
is_known_property(16#18) -> true;
is_known_property(16#19) -> true;
is_known_property(16#1A) -> true;
is_known_property(16#1C) -> true;
is_known_property(16#1F) -> true;
is_known_property(16#21) -> true;
is_known_property(16#22) -> true;
is_known_property(16#23) -> true;
is_known_property(16#24) -> true;
is_known_property(16#25) -> true;
is_known_property(16#26) -> true;
is_known_property(16#27) -> true;
is_known_property(16#28) -> true;
is_known_property(16#29) -> true;
is_known_property(16#2A) -> true;
is_known_property(_) -> false.

parse_varint(B) ->
    parse_varint(B, 0, 0).

parse_varint(<<>>, _Count, _Value) ->
    {error, incomplete_packet};
parse_varint(<<0:1, 0:7, _Rest/binary>>, Count, _Value) when Count > 0 ->
    {error, malformed_packet};
parse_varint(<<0:1, I:7, Rest/binary>>, Count, Value) ->
    {ok, {Value + (I bsl (Count * 7)), Rest}};
parse_varint(<<1:1, I:7, Rest/binary>>, Count, Value) when Count < (?MAX_VARINT_BYTES - 1) ->
    parse_varint(Rest, Count + 1, Value + (I bsl (Count * 7)));
parse_varint(<<1:1, _I:7, _Rest/binary>>, _Count, _Value) ->
    {error, malformed_packet}.

split_topic(Topic) ->
    mqtt_packet_map_topic:validate_topic(Topic).

parse_utf_pair(Bin) ->
    case parse_utf(Bin) of
        {ok, {Key, Bin1}} ->
            case parse_utf(Bin1) of
                {ok, {Val, Rest}} -> {ok, {{Key, Val}, Rest}};
                {error, _} = ErrUTF -> ErrUTF
            end;
        {error, _} = ErrUTF -> ErrUTF
    end.

parse_utf(<<Len:16/big, Str:Len/binary, Rest/binary>>) ->
    {ok, {Str, Rest}};
parse_utf(Bin) when is_binary(Bin) ->
    {error, incomplete_packet}.

parse_bin(<<Len:16/big, Bin:Len/binary, Rest/binary>>) ->
    {ok, {Bin, Rest}};
parse_bin(Bin) when is_binary(Bin) ->
    {error, incomplete_packet}.

parse_bool(0) -> {ok, false};
parse_bool(1) -> {ok, true};
parse_bool(_) -> {error, malformed_packet}.

parse_qos(0) -> {ok, 0};
parse_qos(1) -> {ok, 1};
parse_qos(2) -> {ok, 2};
parse_qos(_) -> {error, malformed_packet}.

