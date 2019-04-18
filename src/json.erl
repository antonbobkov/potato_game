-module(json).

-compile(export_all).
-compile(nowarn_export_all).

%% -include_lib("stdlib/include/assert.hrl").
%% -include_lib("eunit/include/eunit.hrl").

%% -include("potato_records.hrl").

find(Key, Map) when is_atom(Key) ->
    maps:find(atom_to_binary(Key, utf8), Map).

get(Key, Map) when is_atom(Key) ->
    maps:get(atom_to_binary(Key, utf8), Map).

get_str(Key, Map) when is_atom(Key) ->
    binary_to_list(maps:get(atom_to_binary(Key, utf8), Map)).

put(Key, Value, Map) when is_atom(Key) ->
    maps:put(atom_to_binary(Key, utf8), Value, Map).
