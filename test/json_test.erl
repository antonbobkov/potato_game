-module(json_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

basic_test() ->
    ?debugVal(jsx:decode(<<"{\"library\": \"jsx\", \"awesome\": true}">>, [return_maps])),
    ?debugFmt("~n~p~n", [jsx:encode(#{<<"library">> => <<"jsx">>, <<"awesome">> => true})]),
    ?debugFmt("~n~s~n", [jsx:encode(#{<<"library">> => <<"jsx">>, <<"awesome">> => true}, [indent, space])]),
    ok.
