%% @doc Minimal value conversion helpers used by mqtt_packet_map.

%% Copyright 2026 Marc Worrell
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

-module(mqtt_packet_map_convert).

-export([
    to_binary/1
]).

-spec to_binary(term()) -> binary().
to_binary(undefined) ->
    <<>>;
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) ->
    B;
to_binary(I) when is_integer(I) ->
    integer_to_binary(I);
to_binary(F) when is_float(F) ->
    float_to_binary(F, [short]);
to_binary(L) when is_list(L) ->
    iolist_to_binary(L).
