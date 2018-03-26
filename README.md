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

```erlang
% Decode an incoming binary, return the message
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
```

MQTT v5 Specification
=====================

This library follows the following specification:

http://docs.oasis-open.org/mqtt/mqtt/v5.0/cs01/mqtt-v5.0-cs01.html


Usage
=====

Include the mqtt_packet_map directly in your rebar.config:

```erlang
{deps, [
    mqtt_packet_map
]}.
```

Tests
=====

Run tests with:

```bash
make test
```

Sample output:

```
./rebar3 ct --config rebar.test.config
===> Verifying dependencies...
===> Compiling mqtt_packet_map
===> Running Common Test suites...
%%% mqtt_packet_map_SUITE ==> variable_byte_integer: OK
%%% mqtt_packet_map_SUITE ==> partial_packet: OK
%%% mqtt_packet_map_SUITE ==> connect_v5: OK
%%% mqtt_packet_map_SUITE ==> connect_v5_full: OK
%%% mqtt_packet_map_SUITE ==> connack_v5: OK
%%% mqtt_packet_map_SUITE ==> publish_v5: OK
%%% mqtt_packet_map_SUITE ==> puback_et_al_v5: OK
%%% mqtt_packet_map_SUITE ==> subscribe_v5: OK
%%% mqtt_packet_map_SUITE ==> suback_v5: OK
%%% mqtt_packet_map_SUITE ==> unsubscribe_v5: OK
%%% mqtt_packet_map_SUITE ==> unsuback_v5: OK
%%% mqtt_packet_map_SUITE ==> pingreq: OK
%%% mqtt_packet_map_SUITE ==> pingresp: OK
%%% mqtt_packet_map_SUITE ==> disconnect_v5: OK
%%% mqtt_packet_map_SUITE ==> auth_v5: OK
All 15 tests passed.
```

Packet Types
============

Below is the list of packet types and their fields.

Fields that are omitted are set to their defaults.

For example, `reason_code`, `qos`, and `packet_id` will
all default to `0`.

Some fields, like the topic for publish, are obligatory.
The encoder will crash if you leave out oblibatory fields.

Topics
------

Topics are parsed as lists (i.e. they are split on the `/` separator).

When encoding, both a binary and a list are accepted.

So the following are acceptable topics for the encoder:

 * `<<"foo/bar">>`
 * `[ <<"foo">> | <<"bar">> ]`

Which are both decoded as:

 * `[ <<"foo">> | <<"bar">> ]`


Properties
----------

The (optional) properties of a package are represented as a map.

Known properties have an atom as key, user properties a binary.

Below is an example map with all properties and one user (`<<"myuserprop">>`) property. The example values are random and
have no bearing in reality.

```Erlang
#{
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
    <<"myuserprop">> => <<"foobar">>,
    maximum_packet_size => 1234567,
    wildcard_subscription_available => true,
    subscription_identifier_available => false,
    shared_subscription_available => true
}.
```

The `subscription_identifier` can be present multiple times, making it either a single integer or a list of integers.

CONNECT
-------

Minimal:

```Erlang
#{ type => connect }
```

Complete:

```erlang
 #{
    type => connect,
    client_id => <<"foobar">>,
    username => <<"someone">>,
    password => <<"secret">>,
    clean_start => true,
    keep_alive => 120,
    properties => #{
        <<"foo">> => <<"bar">>,
        will_delay_interval => 10
    },
    will_flag => true,
    will_payload => <<>>,
    will_properties => #{},
    will_qos => 0,
    will_retain => false,
    will_topic => [ <<"good">>, <<"bye">> ]
}
```

CONNACK
-------

Minimal:

```Erlang
#{ type = connack }
```

Complete:

```erlang
#{
    type => connack,
    reason_code => 16#80,
    session_present => true,
    properties => #{
        <<"foo">> => <<"bar">>
    }
}
```

PUBLISH
-------

Minimal:

```Erlang
#{
    type => publish,
    topic => [ <<"foo">>, <<"bar">>, <<"la">> ]
}
```

Complete:

```erlang
#{
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
}
```

PUBACK / PUBREC / PUBREL / PUBCOMP
----------------------------------

These for packets are the same. Only the type code is different.

Minimal:

```Erlang
% Type is one of: puback, pubrec, pubrel, or pubcomp
#{ type = puback }
```

Complete:

```erlang
#{
    type => puback,
    reason_code => 16#81,
    packet_id => 4321,
    properties => #{
        <<"bar">> => <<"fooooo">>
    }
}
```


SUBSCRIBE
-------

The topics subscribed to are either maps with options or just a topic.

Minimal:

```Erlang
#{
    type => subscribe,
    topics => [
        [ <<"foo1">>, <<"bar">> ]
    ]
}
```

Complete:

```erlang
#{
    type => subscribe,
    packet_id => 1234,
    topics => [
        #{
            topic => [ "foo1", "bar" ],
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
}
```

SUBACK
-------

All acknowledgements are tuples `{ok, QoS}` or `{error, ReasonCode}`.

Minimal:

```Erlang
#{
    type => suback,
    acks => [
        {ok, 0}
    ]
}
```

Complete (for four acks):

```erlang
#{
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
}
```

UNSUBSCRIBE
-----------

Minimal:

```Erlang
#{
    type => unsubscribe,
    topics => [
        [ <<"foo">>, <<"bar">> ]
    ]
}
```

Complete:

```erlang
#{
    type => unsubscribe,
    packet_id => 42,
    topics => [
        <<"foo1/bar">>,
        [ <<"foo2">>, <<"bar">> ]
    ],
    properties => #{
        <<"foo">> => <<"bar">>
    }
}
```

UNSUBACK
-----------

The acknowledgements are one of:

 * `{ok, found}`
 * `{ok, notfound}`
 * `{error, ReasonCode}`

Minimal:

```Erlang
#{
    type => unsuback,
    acks => [
        {ok, found}
    ]
}
```

Complete (for three acks):

```erlang
#{
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
}
```

PINGREQ
-------

No special fields.

```Erlang
#{ type => pingreq }
```

PINGRESP
--------

No special fields

```Erlang
#{ type => pingresp }
```

DISCONNECT
----------

The default reason code for disconnects is `0`.

Minimal:

```Erlang
#{ type => disconnect }
```

Complete:

```erlang
#{
    type => disconnect,
    reason_code => 16#81,
    properties => #{
        <<"foo">> => <<"bar">>
    }
}
```

AUTH
----

Minimal:

```Erlang
#{ type => auth }
```

Complete:

```erlang
{
    type => auth,
    reason_code => 16#80,
    properties => #{
        <<"foo">> => <<"bar">>,
        authentication_method => <<"...">>,
        authentication_data => <<"...">>
    }
}
```


License
=======

This library is licensed under the Apache License version 2.0.

See the LICENSE file.
