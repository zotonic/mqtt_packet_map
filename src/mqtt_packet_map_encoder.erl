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

-module(mqtt_packet_map_encoder).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    encode/2
]).

% testing
-export([
    varint/1
    ]).

-include("mqtt_packet_map_defs.hrl").

-spec encode( mqtt_encoder:mqtt_version(), mqtt_encoder:mqtt_packet() ) -> {ok, binary()} | {error, term()}.
encode(MQTTVersion, #{ type := connect } = Msg) ->
    WillFlag = maps:get(will_flag, Msg, false),
    WillQoS = maps:get(will_qos, Msg, 0),
    KeepAlive = maps:get(keep_alive, Msg, 0),
    Variable = [
        bin(magic(MQTTVersion)),
        <<
            MQTTVersion:8,
            (opt(username, Msg)):1, (opt(password, Msg)):1,
            (bool(will_retain, Msg)):1, (WillQoS):2, (bool(WillFlag)):1,
            (bool(clean_start, Msg)):1, 0:1,
            KeepAlive:16/big
        >>,
        case MQTTVersion of
            ?MQTTv5 -> serialize_properties(Msg);
            _ -> <<>>
        end,
        bin(maps:get(client_id, Msg, <<>>)),
        case WillFlag of
            true ->
                [
                    case MQTTVersion of
                        ?MQTTv5 -> serialize_properties(Msg, will_properties);
                        _ -> <<>>
                    end,
                    topic_bin(maps:get(will_topic, Msg)),
                    bin(maps:get(will_payload, Msg, <<>>))
                ];
            false ->
                <<>>
        end,
        case maps:get(username, Msg, undefined) of
            undefined -> <<>>;
            Username -> bin(Username)
        end,
        case maps:get(password, Msg, undefined) of
            undefined -> <<>>;
            Password -> bin(Password)
        end
    ],
    packet(<<?CONNECT:4, 0:4>>, Variable);
encode(MQTTVersion, #{ type := connack } = Msg) ->
    Variable = [
        << 0:7, (bool(session_present, Msg)):1, (maps:get(reason_code, Msg, 0)):8 >>,
        case MQTTVersion of
            ?MQTTv5 -> serialize_properties(Msg);
            _ -> <<>>
        end
    ],
    packet(<<?CONNACK:4, 0:4>>, Variable);
encode(MQTTVersion, #{ type := publish } = Msg) ->
    QoS = maps:get(qos, Msg, 0),
    Variable = [
        topic_bin(maps:get(topic, Msg)),
        case QoS of
            0 -> <<>>;
            _ -> << (maps:get(packet_id, Msg, 0)):16/big >>
        end,
        case MQTTVersion of
            ?MQTTv5 -> serialize_properties(Msg);
            _ -> <<>>
        end,
        case maps:get(payload, Msg, undefined) of
            undefined -> <<>>;
            B when is_binary(B) -> B
        end
    ],
    Fixed = << ?PUBLISH:4, (bool(dup, Msg)):1, QoS:2, (bool(retain, Msg)):1 >>,
    packet(Fixed, Variable);
encode(MQTTVersion, #{ type := P } = Msg)
    when P =:= puback; P =:= pubrec; P =:= pubrel; P =:= pubcomp ->
    Variable = [
        << (maps:get(packet_id, Msg, 0)):16/big >>,
        case MQTTVersion of
            ?MQTTv5 ->
                [
                    << (maps:get(reason_code, Msg, 0)):8 >>,
                    serialize_properties(Msg)
                ];
            _ ->  <<>>
        end
    ],
    TP = case P of
        puback -> ?PUBACK;
        pubrec -> ?PUBREC;
        pubrel -> ?PUBREL;
        pubcomp -> ?PUBCOMP
    end,
    packet(<<TP:4, 0:4>>, Variable);
encode(MQTTVersion, #{ type := subscribe } = Msg) ->
    Variable = [
        << (maps:get(packet_id, Msg, 0)):16/big >>,
        case MQTTVersion of
            ?MQTTv5 -> serialize_properties(Msg);
            _ ->  <<>>
        end,
        serialize_subscribe_topics(maps:get(topics, Msg))
    ],
    packet(<<?SUBSCRIBE:4, 0:2, 1:1, 0:1>>, Variable);
encode(MQTTVersion, #{ type := suback } = Msg) ->
    Variable = [
        << (maps:get(packet_id, Msg, 0)):16/big >>,
        case MQTTVersion of
            ?MQTTv5 -> serialize_properties(Msg);
            _ ->  <<>>
        end,
        serialize_acks(maps:get(acks, Msg))
    ],
    packet(<<?SUBACK:4, 0:4>>, Variable);
encode(MQTTVersion, #{ type := unsubscribe } = Msg) ->
    Variable = [
        << (maps:get(packet_id, Msg, 0)):16/big >>,
        case MQTTVersion of
            ?MQTTv5 -> serialize_properties(Msg);
            _ ->  <<>>
        end,
        serialize_unsubscribe_topics(maps:get(topics, Msg))
    ],
    packet(<<?UNSUBSCRIBE:4, 0:2, 1:1, 0:1>>, Variable);
encode(MQTTVersion, #{ type := unsuback } = Msg) ->
    Variable = [
        << (maps:get(packet_id, Msg, 0)):16/big >>,
        case MQTTVersion of
            ?MQTTv5 -> serialize_properties(Msg);
            _ ->  <<>>
        end,
        serialize_unacks(maps:get(acks, Msg))
    ],
    packet(<<?UNSUBACK:4, 0:4>>, Variable);
encode(_MQTTVersion, #{ type := pingreq }) ->
    packet(<<?PINGREQ:4, 0:4>>, <<>>);
encode(_MQTTVersion, #{ type := pingresp }) ->
    packet(<<?PINGRESP:4, 0:4>>, <<>>);
encode(MQTTVersion, #{ type := disconnect } = Msg) ->
    ReasonCode = maps:get(reason_code, Msg, 0),
    Properties = maps:get(properties, Msg, #{}),
    Variable = case {ReasonCode, Properties} of
        {0, #{}} ->
            <<>>;
        _ ->
            [
                << (maps:get(reason_code, Msg, 0)):8 >>,
                case MQTTVersion of
                    ?MQTTv5 -> serialize_properties(Msg);
                    _ ->  <<>>
                end
            ]
    end,
    packet(<<?DISCONNECT:4, 0:4>>, Variable);
encode(?MQTTv5, #{ type := auth } = Msg) ->
    ReasonCode = maps:get(reason_code, Msg, 0),
    Properties = maps:get(properties, Msg, #{}),
    Variable = case {ReasonCode, Properties} of
        {0, #{}} ->
            <<>>;
        _ ->
            [
                << (maps:get(reason_code, Msg, 0)):8 >>,
                serialize_properties(Msg)
            ]
    end,
    packet(<<?AUTH:4, 0:4>>, Variable).


packet(Header, Variable) ->
    VB = iolist_to_binary(Variable),
    Size = size(VB),
    true = (Size =< ?MAX_PACKET_SIZE),
    {ok, << Header/binary, (varint(Size))/binary, VB/binary >>}.



serialize_subscribe_topics(Topics) ->
    lists:map( fun serialize_subscribe_topic/1, Topics ).

serialize_subscribe_topic(#{ topic := Name } = T) ->
    RH = maps:get(retain_handling, T, 0),
    RAP = bool(retain_as_published, T),
    NL = bool(no_local, T),
    QoS = maps:get(qos, T, 0),
    [
        topic_bin(Name),
        << 0:2, RH:2, RAP:1, NL:1, QoS:2 >>
    ];
serialize_subscribe_topic(T) when is_binary(T); is_list(T) ->
    serialize_subscribe_topic(#{ topic => T }).

serialize_unsubscribe_topics(Topics) ->
    lists:map(
        fun(Topic) ->
            topic_bin(Topic)
        end,
        Topics).

serialize_acks(Acks) ->
    lists:map(
        fun
            ({ok, QoS}) when QoS >= 0, QoS =< 2 -> << 0:6, QoS:2 >>;
            ({error, Reason}) when Reason >= 16#80, Reason =< 16#ff -> << Reason:8 >>
        end,
        Acks).

serialize_unacks(UnAcks) ->
    lists:map(
        fun
            ({ok, found}) -> 0;
            ({ok, notfound}) -> 17;
            ({error, Reason}) when Reason >= 16#80, Reason =< 16#ff -> << Reason:8 >>
        end,
        UnAcks).


serialize_properties(Msg) ->
    serialize_properties(Msg, properties).

serialize_properties(Msg, Props) ->
    Ser = maps:fold(
        fun( K, V, Acc ) ->
            [ serprop(K, V) | Acc ]
        end,
        [],
        maps:get(Props, Msg, #{})),
    SerBin = iolist_to_binary(Ser),
    << (varint(size(SerBin)))/binary, SerBin/binary >>.


-define(i16(V), is_integer(V), (V >= 0 andalso V =< 65535)).
-define(i32(V), is_integer(V), (V >= 0 andalso V =< 4294967295)).
-define(qos(V), is_integer(V), (V >= 0 andalso V =< 2)).

serprop(payload_format_indicator, Val) ->          <<16#01, (bool(Val)):8>>;
serprop(message_expiry_interval, Val) when ?i32(Val) -> <<16#02, Val:32/big>>;
serprop(content_type, Val) ->                      <<16#03, (bin(Val))/binary>>;
serprop(response_topic, Val) ->                    <<16#08, (topic_bin(Val))/binary>>;
serprop(correlation_data, Val) ->                  <<16#09, (bin(Val))/binary>>;
serprop(subscription_identifier, Vs) when is_list(Vs) ->
    lists:map(fun(V) -> <<16#0B, (varint(V))/binary>> end, Vs);
serprop(subscription_identifier, Val) ->           <<16#0B, (varint(Val))/binary>>;
serprop(session_expiry_interval, Val) when ?i32(Val) -> <<16#11, Val:32/big>>;
serprop(assigned_client_identifier, Val) ->        <<16#12, (bin(Val))/binary>>;
serprop(server_keep_alive, Val) when ?i16(Val) ->   <<16#13, Val:16/big>>;
serprop(authentication_method, Val) ->             <<16#15, (bin(Val))/binary>>;
serprop(authentication_data, Val) ->               <<16#16, (bin(Val))/binary>>;
serprop(request_problem_information, Val) ->       <<16#17, (bool(Val)):8>>;
serprop(will_delay_interval, Val) when ?i32(Val) -> <<16#18, Val:32/big>>;
serprop(request_response_information, Val) ->      <<16#19, (bool(Val)):8>>;
serprop(response_information, Val) ->              <<16#1A, (bin(Val))/binary>>;
serprop(server_reference, Val) ->                  <<16#1C, (bin(Val))/binary>>;
serprop(reason_string, Val) ->                     <<16#1F, (bin(Val))/binary>>;
serprop(receive_maximum, Val) when ?i16(Val) ->    <<16#21, Val:16/big>>;
serprop(topic_alias_maximum, Val) when ?i16(Val)-> <<16#22, Val:16/big>>;
serprop(topic_alias, Val) when ?i16(Val) ->        <<16#23, Val:16/big>>;
serprop(maximum_qos, Val) when ?qos(Val) ->        <<16#24, Val:8>>;
serprop(retain_available, Val) ->                  <<16#25, (bool(Val)):8>>;
serprop(UserProp, Val) when is_binary(UserProp) -> <<16#26, (bin(UserProp))/binary, (bin(Val))/binary>>;
serprop(maximum_packet_size, Val) when ?i32(Val)-> <<16#27, Val:32/big>>;
serprop(wildcard_subscription_available, Val) ->   <<16#28, (bool(Val)):8>>;
serprop(subscription_identifier_available, Val) -> <<16#29, (bool(Val)):8>>;
serprop(shared_subscription_available, Val) ->     <<16#2A, (bool(Val)):8>>.


topic_bin(B) when is_binary(B) ->
    bin(B);
topic_bin([ H ]) ->
    bin( topic_bin_1(H) );
topic_bin([ H | T ]) when is_binary(H) ->
    bin( iolist_to_binary([ topic_bin_1(H), [ [ $/, topic_bin_1(S) ] || S <- T ] ]) ).

topic_bin_1(B) when is_binary(B) -> B;
topic_bin_1(N) when is_integer(N) -> integer_to_binary(N);
topic_bin_1(A) when is_atom(A) -> atom_to_binary(A, utf8).

bin(undefined) ->
    bin(<<>>);
bin(Bin) ->
    <<(size(Bin)):16/big, Bin/binary>>.

varint(I) when I =< 16#7f, I >= 0 ->
    <<0:1, I:7>>;
varint(I) when I =< ?MAX_VARINT, I >= 0 ->
    <<1:1, (I band 16#7f):7, (varint(I bsr 7))/binary>>.

magic(?MQTTv3) -> ?PROTOCOL_NAME_3;
magic(_) -> ?PROTOCOL_NAME.

bool(K, Msg) ->
    bool(maps:get(K, Msg, undefined)).

bool(undefined) -> 0; % reserved
bool(0) -> 0;
bool(1) -> 1;
bool(true) -> 1;
bool(false) -> 0.

opt(K, Msg) ->
    opt(maps:get(K, Msg, undefined)).

opt(undefined) -> 0;
opt(_) -> 1.
