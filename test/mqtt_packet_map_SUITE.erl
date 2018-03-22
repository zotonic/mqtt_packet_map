-module(mqtt_packet_map_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        variable_byte_integer,
        partial_packet,
        connect_v5,
        connect_v5_full,
        connack_v5,
        publish_v5,
        puback_et_al_v5,
        subscribe_v5,
        suback_v5,
        unsubscribe_v5,
        unsuback_v5,
        pingreq,
        pingresp,
        disconnect_v5,
        auth_v5
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

variable_byte_integer(_Config) ->
    <<16#01>> = mqtt_packet_map_encoder:varint(1),
    <<16#7F>> = mqtt_packet_map_encoder:varint(127),
    <<16#80, 16#01>> = mqtt_packet_map_encoder:varint(128),
    <<16#80, 16#80, 16#80, 16#01>> = mqtt_packet_map_encoder:varint(2097152),
    {0, <<>>} = mqtt_packet_map_decoder:parse_varint( mqtt_packet_map_encoder:varint(0) ),
    {1, <<>>} = mqtt_packet_map_decoder:parse_varint( mqtt_packet_map_encoder:varint(1) ),
    {127, <<>>} = mqtt_packet_map_decoder:parse_varint( mqtt_packet_map_encoder:varint(127) ),
    {128, <<>>} = mqtt_packet_map_decoder:parse_varint( mqtt_packet_map_encoder:varint(128) ),
    {1234567890, <<>>} = mqtt_packet_map_decoder:parse_varint( mqtt_packet_map_encoder:varint(1234567890) ),
    {16#7fffffff, <<>>} = mqtt_packet_map_decoder:parse_varint( mqtt_packet_map_encoder:varint(16#7fffffff) ),
    ok.

partial_packet(_Config) ->
    {error, incomplete_packet} = mqtt_packet_map:decode(<<240,2,0>>),
    {error, malformed_header} = mqtt_packet_map:decode(<<255,255,255,255,255,255,255,255>>),
    {ok, {_, <<1,2,3>>}} = mqtt_packet_map:decode(<<240,2,0,0,1,2,3>>),
    ok.

connect_v5(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{ type => connect } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        clean_start := false,
        client_id := <<>>,
        keep_alive := 0,
        password := undefined,
        properties := #{},
        protocol_name := <<"MQTT">>,
        protocol_version := 5,
        type := connect,
        username := undefined,
        will_flag := false,
        will_payload := undefined,
        will_properties := #{},
        will_qos := 0,
        will_retain := false,
        will_topic := undefined
    } = M,
    ok.

connect_v5_full(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{
        type => connect,
        client_id => <<"foobar">>,
        clean_start => true,
        keep_alive => 123,
        password => <<"secret">>,
        username => <<"someone">>,
        properties => #{
            payload_format_indicator => true,
            message_expiry_interval => 1,
            content_type => <<"text/plain">>,
            response_topic => [ <<"response">>, <<"topic">> ],
            correlation_data => <<"corrdata">>,
            subscription_identifier => 2,
            session_expiry_interval => 3,
            assigned_client_identifier => <<"assclientid">>,
            server_keep_alive => 4,
            authentication_method => <<"authmethod">>,
            authentication_data => <<"authdata">>,
            request_problem_information => true,
            will_delay_interval => 5,
            request_response_information => false,
            response_information => <<"respinfo">>,
            server_reference => <<"servref">>,
            reason_string => <<"reason">>,
            receive_maximum => 12345,
            topic_alias_maximum => 6,
            topic_alias => 7,
            maximum_qos => 2,
            retain_available => true,
            <<"foo">> => <<"bar">>,
            maximum_packet_size => 1234567,
            wildcard_subscription_available => true,
            subscription_identifier_available => false,
            shared_subscription_available => true
        },
        will_flag => true,
        will_topic => [ <<"good">>, <<"bye">> ]
    } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        clean_start := true,
        client_id := <<"foobar">>,
        keep_alive := 123,
        password := <<"secret">>,
        properties := #{
            payload_format_indicator := true,
            message_expiry_interval := 1,
            content_type := <<"text/plain">>,
            response_topic := [ <<"response">>, <<"topic">> ],
            correlation_data := <<"corrdata">>,
            subscription_identifier := 2,
            session_expiry_interval := 3,
            assigned_client_identifier := <<"assclientid">>,
            server_keep_alive := 4,
            authentication_method := <<"authmethod">>,
            authentication_data := <<"authdata">>,
            request_problem_information := true,
            will_delay_interval := 5,
            request_response_information := false,
            response_information := <<"respinfo">>,
            server_reference := <<"servref">>,
            reason_string := <<"reason">>,
            receive_maximum := 12345,
            topic_alias_maximum := 6,
            topic_alias := 7,
            maximum_qos := 2,
            retain_available := true,
            <<"foo">> := <<"bar">>,
            maximum_packet_size := 1234567,
            wildcard_subscription_available := true,
            subscription_identifier_available := false,
            shared_subscription_available := true
        },
        protocol_name := <<"MQTT">>,
        protocol_version := 5,
        type := connect,
        username := <<"someone">>,
        will_flag := true,
        will_payload := <<>>,
        will_properties := #{},
        will_qos := 0,
        will_retain := false,
        will_topic := [ <<"good">>, <<"bye">> ]
    } = M,
    ok.

connack_v5(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{ type => connack } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        type := connack,
        reason_code := 0,
        session_present := false,
        properties := #{}
    } = M,
    {ok, B1} = mqtt_packet_map:encode( #{
        type => connack,
        reason_code => 16#80,
        session_present => true,
        properties => #{
            <<"foo">> => <<"bar">>
        }
    } ),
    {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
    #{
        type := connack,
        reason_code := 16#80,
        session_present := true,
        properties := #{
            <<"foo">> := <<"bar">>
        }
    } = M1,
    ok.

publish_v5(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{
        type => publish,
        topic => [ <<"foo">>, <<"bar">>, <<"la">> ]
    } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        type := publish,
        topic := [ <<"foo">>, <<"bar">>, <<"la">> ],
        qos := 0,
        dup := false,
        retain := false,
        packet_id := undefined,
        payload := <<>>,
        properties := #{}
    } = M,
    {ok, B1} = mqtt_packet_map:encode( #{
        type => publish,
        topic => [ <<"foo">>, <<"bar">>, <<"la">> ],
        qos => 2,
        dup => true,
        retain => true,
        packet_id => 1234,
        payload => <<"aloha">>,
        properties => #{
            <<"foo">> => <<"bar">>
        }
    } ),
    {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
    #{
        type := publish,
        topic := [ <<"foo">>, <<"bar">>, <<"la">> ],
        qos := 2,
        dup := true,
        retain := true,
        packet_id := 1234,
        payload := <<"aloha">>,
        properties := #{
            <<"foo">> := <<"bar">>
        }
    } = M1,
    % Packet-id is undefined when qos is 0
    {ok, B2} = mqtt_packet_map:encode( #{
        type => publish,
        topic => [ <<"foo">>, <<"bar">>, <<"la">> ],
        qos => 0,
        packet_id => 1234
    } ),
    {ok, {M2, <<>>}} = mqtt_packet_map:decode(B2),
    #{
        type := publish,
        topic := [ <<"foo">>, <<"bar">>, <<"la">> ],
        qos := 0,
        packet_id := undefined
    } = M2,
    % Multiple subscription identifiers
    {ok, B3} = mqtt_packet_map:encode( #{
        type => publish,
        topic => [ <<"foo">>, <<"bar">>, <<"la">> ],
        properties => #{
            subscription_identifier => [ 1, 2, 3 ]
        }
    } ),
    {ok, {M3, <<>>}} = mqtt_packet_map:decode(B3),
    #{
        type := publish,
        topic := [ <<"foo">>, <<"bar">>, <<"la">> ],
        properties := #{
            subscription_identifier := [ 1, 2, 3 ]
        }
    } = M3,
    ok.

puback_et_al_v5(_Config) ->
    lists:foreach(
        fun(Type) ->
            {ok, B} = mqtt_packet_map:encode( #{
                type => Type
            } ),
            {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
            #{
                type := Type,
                packet_id := 0,
                reason_code := 0,
                properties := #{}
            } = M,
            {ok, B1} = mqtt_packet_map:encode( #{
                type => Type,
                reason_code => 16#81,
                packet_id => 4321,
                properties => #{
                    <<"bar">> => <<"fooooo">>
                }
            } ),
            {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
            #{
                type := Type,
                reason_code := 16#81,
                packet_id := 4321,
                properties := #{
                    <<"bar">> := <<"fooooo">>
                }
            } = M1
        end,
        [ puback, pubrec, pubrel, pubcomp ]).

subscribe_v5(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{
        type => subscribe,
        topics => [
            #{
                topic => <<"foo1/bar">>
            },
            #{
                topic => [ <<"foo2">>, <<"bar">> ]
            }
        ]
    } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        type := subscribe,
        packet_id := 0,
        topics := [
            #{
                topic := [ <<"foo1">>, <<"bar">> ]
            },
            #{
                topic := [ <<"foo2">>, <<"bar">> ],
                no_local := false,
                qos := 0,
                retain_as_published := false,
                retain_handling := 0
            }
        ],
        properties := #{}
    } = M,
    {ok, B1} = mqtt_packet_map:encode( #{
        type => subscribe,
        packet_id => 1234,
        topics => [
            #{
                topic => <<"foo1/bar">>,
                no_local => true,
                qos => 2,
                retain_as_published => true,
                retain_handling => 2
            },
            #{
                topic => [ <<"foo2">>, <<"bar">> ]
            }
        ],
        properties => #{
            <<"foo">> => <<"bar">>
        }
    } ),
    {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
    #{
        type := subscribe,
        packet_id := 1234,
        topics := [
            #{
                topic := [ <<"foo1">>, <<"bar">> ],
                no_local := true,
                qos := 2,
                retain_as_published := true,
                retain_handling := 2
            },
            #{
                topic := [ <<"foo2">>, <<"bar">> ],
                no_local := false,
                qos := 0,
                retain_as_published := false,
                retain_handling := 0
            }
        ],
        properties := #{
            <<"foo">> := <<"bar">>
        }
    } = M1,
    ok.

suback_v5(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{ type => suback, acks => [] } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        type := suback,
        packet_id := 0,
        acks := [],
        properties := #{}
    } = M,
    {ok, B1} = mqtt_packet_map:encode( #{
        type => suback,
        packet_id => 12345,
        acks => [
            {ok, 2},
            {ok, 0},
            {ok, 1},
            {error, 16#80}
        ],
        properties => #{
            <<"foo">> => <<"bar">>
        }
    } ),
    {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
    #{
        type := suback,
        packet_id := 12345,
        acks := [
            {ok, 2},
            {ok, 0},
            {ok, 1},
            {error, 16#80}
        ],
        properties := #{
            <<"foo">> := <<"bar">>
        }
    } = M1,
    ok.

unsubscribe_v5(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{
        type => unsubscribe,
        topics => [
            <<"foo1/bar">>,
            [ <<"foo2">>, <<"bar">> ]
        ]
    } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        type := unsubscribe,
        packet_id := 0,
        topics := [
            [ <<"foo1">>, <<"bar">> ],
            [ <<"foo2">>, <<"bar">> ]
        ],
        properties := #{}
    } = M,
    {ok, B1} = mqtt_packet_map:encode( #{
        type => unsubscribe,
        packet_id => 42,
        topics => [
            <<"foo1/bar">>,
            [ <<"foo2">>, <<"bar">> ]
        ],
        properties => #{
            <<"foo">> => <<"bar">>
        }
    } ),
    {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
    #{
        type := unsubscribe,
        packet_id := 42,
        topics := [
            [ <<"foo1">>, <<"bar">> ],
            [ <<"foo2">>, <<"bar">> ]
        ],
        properties := #{
            <<"foo">> := <<"bar">>
        }
    } = M1,
    ok.

unsuback_v5(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{ type => unsuback, acks => [] } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        type := unsuback,
        packet_id := 0,
        acks := [],
        properties := #{}
    } = M,
    {ok, B1} = mqtt_packet_map:encode( #{
        type => unsuback,
        packet_id => 12345,
        acks => [
            {ok, found},
            {ok, notfound},
            {error, 16#80}
        ],
        properties => #{
            <<"foo">> => <<"bar">>
        }
    } ),
    {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
    #{
        type := unsuback,
        packet_id := 12345,
        acks := [
            {ok, found},
            {ok, notfound},
            {error, 16#80}
        ],
        properties := #{
            <<"foo">> := <<"bar">>
        }
    } = M1,
    ok.

pingreq(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{ type => pingreq } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{ type := pingreq } = M,
    ok.

pingresp(_Config) ->
    {ok, B} = mqtt_packet_map:encode( #{ type => pingresp } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{ type := pingresp } = M,
    ok.

disconnect_v5(_Config) ->
    {ok, <<224,0>> = B} = mqtt_packet_map:encode( #{ type => disconnect } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        type := disconnect,
        reason_code := 0,
        properties := #{}
    } = M,
    {ok, B1} = mqtt_packet_map:encode( #{
        type => disconnect,
        reason_code => 16#81,
        properties => #{
            <<"foo">> => <<"bar">>
        }
    } ),
    {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
    #{
        type := disconnect,
        reason_code := 16#81,
        properties := #{
            <<"foo">> := <<"bar">>
        }
    } = M1,
    ok.

auth_v5(_Config) ->
    {ok, <<16#f0, 0>> = B} = mqtt_packet_map:encode( #{ type => auth } ),
    {ok, {M, <<>>}} = mqtt_packet_map:decode(B),
    #{
        type := auth,
        reason_code := 0,
        properties := #{}
    } = M,
    {ok, B1} = mqtt_packet_map:encode( #{
        type => auth,
        reason_code => 16#80,
        properties => #{
            <<"foo">> => <<"bar">>
        }
    } ),
    {ok, {M1, <<>>}} = mqtt_packet_map:decode(B1),
    #{
        type := auth,
        reason_code := 16#80,
        properties := #{
            <<"foo">> := <<"bar">>
        }
    } = M1,
    ok.
