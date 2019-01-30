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

    jsx:decode(File, [return_maps]),

    {ok, File2} = file:read_file(<<"test/test_config_3.json">>),
    _Obj = jsx:decode(File2, [return_maps]),

    %% ?debugVal(Obj),

    ok.
