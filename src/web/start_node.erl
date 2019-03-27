-module(start_node). 
-export([start/1]). 

start(ArgList) ->
    io:format("args: ~n~p~n", [ArgList]),
    ok.
