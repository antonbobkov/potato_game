-module(test_sws).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([start/0]).


start() ->
    io:format("start call ~n",[]),
    sws:start(fun basic_handle/2, 8045).

basic_handle(_S, ReqList) ->
    io:format("handle call ~n",[]),
    lists:foreach(
      fun(E) -> 
	      io:format("~p~n",[E])
      end, 
      ReqList),
    {200, [], "<pre>\n" ++ io_lib:format("~p~n~n Time: ~p~n~n System Info: ~p~n", [ReqList, erlang:localtime(), os:cmd("uname -a")]) ++ "</pre>\n"}.


 
basic_handle_test()->
    basic_handle(none, 
		 [
		  {method,'GET'},
		  {uri,<<"/">>},
		  {version,{1,1}},
		  {headers,[{'Connection',<<"Keep-Alive">>},
			    {'Host',<<"localhost:8045">>},
			    {'Accept-Encoding',<<"identity">>},
			    {'Accept',<<"*/*">>},
			    {'User-Agent',<<"Wget/1.19.4 (linux-gnu)">>}]}
		 ]
		).
