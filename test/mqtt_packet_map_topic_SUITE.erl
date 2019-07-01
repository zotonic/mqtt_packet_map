-module(mqtt_packet_map_topic_SUITE).

-compile([export_all, nowarn_export_all]).

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
        validate_topic,
        flatten_topic,
        invalid_topic
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

validate_topic(_Config) ->
    {ok, [ <<"foo">> ]} = mqtt_packet_map_topic:validate_topic(<<"foo">>),
    {ok, [ <<"foo">>, <<"bar">> ]} = mqtt_packet_map_topic:validate_topic(<<"foo/bar">>),

    % Slashes
    {ok, [ <<>>, <<>> ]} = mqtt_packet_map_topic:validate_topic(<<"/">>),
    {ok, [ <<>>, <<"foo">>, <<"bar">> ]} = mqtt_packet_map_topic:validate_topic(<<"/foo/bar">>),
    {ok, [ <<>>, <<"foo">>, <<>>, <<"bar">>, <<>> ]} = mqtt_packet_map_topic:validate_topic(<<"/foo//bar/">>),

    % Wildcards, mapped to atoms
    {ok, [ <<"foo">>, '+', <<"bar">>, '#' ]} = mqtt_packet_map_topic:validate_topic(<<"foo/+/bar/#">>),

    % Type mappings
    {ok, [ '#' ]} = mqtt_packet_map_topic:validate_topic(<<"#">>),
    {ok, [ <<"model">>, <<"rsc">>, <<"1">> ]} = mqtt_packet_map_topic:validate_topic([ <<"model">>, <<"rsc">>, 1 ]),
    ok.

flatten_topic(_Config) ->
    <<"foo">> = mqtt_packet_map_topic:flatten_topic([ <<"foo">> ]),
    <<"/foo//bar">> = mqtt_packet_map_topic:flatten_topic([ <<>>, <<"foo">>, <<>>, <<"bar">> ]),
    <<"foo/+/bar/#">> = mqtt_packet_map_topic:flatten_topic([ <<"foo">>, '+', <<"bar">>, '#' ]),
    ok.

invalid_topic(_Config) ->
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic(<<>>),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic([]),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic(<<0>>),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic(<<"a/a+b/c">>),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic(<<"a/#/c">>),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic(<<"a/#b/c">>),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic([ <<"a">>, '#', <<"b">> ]),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic([ <<"a">>, <<"#">>, <<"b">> ]),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic([ <<"a">>, <<"a#">>, <<"b">> ]),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic([ <<"b", 0, "c">> ]),
    {error, invalid_topic} = mqtt_packet_map_topic:validate_topic([ <<"b", 255, "c">> ]),
    ok.
