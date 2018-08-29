-module(json_test).

-export([cmd/0]).
-import(t2j, [t2j/1]).

print(S) ->
    io:format("~s~n", [S]).

cmd() ->
    print(t2j:t2j({"K",{{key, value},{key2,[{key3, true}, {key4, value4}, {key5, 5.333}]}}})),
    print(t2j:t2j({"K",{{key, <<"value">>},{key2,["a",null,"c",4]}}})),
    print(t2j:t2j( #{hi => 123} )),
    print(t2j:t2j( #{hi => 123, bye => 321, mp => #{a => 1, b => 2}} )),
    ok.

