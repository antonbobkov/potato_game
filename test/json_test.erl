-module(json_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

basic_test() ->
    jsx:decode(<<"{\"library\": \"jsx\", \"awesome\": true}">>
	      , [return_maps]),

    jsx:encode(#{<<"library">> => <<"jsx">>, <<"awesome">> => true}),

    jsx:encode(#{<<"library">> => <<"jsx">>, <<"awesome">> => true}, 
	       [indent, space]),

    {ok, File} = file:read_file("test/json.json"),

    _Obj = jsx:decode(File, [return_maps]),

    %% ?debugVal(Obj),

    ok.
