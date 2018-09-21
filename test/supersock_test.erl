-module(supersock_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

connect() ->
  spawn_link(fun() ->
    {ok, Sock} = gen_tcp:connect("localhost", 8384, [binary, {active, false}]),
    {ok, Msg} = gen_tcp:recv(Sock, 0),
    ?debugFmt("recv ~p~n",[Msg]),
    gen_tcp:send(Sock, "handshake_reply"),
    gen_tcp:close(Sock)
  end).

supersock_test() ->
  supersocket:start_socket_server(),
  potato_haskell:for(fun() -> connect() end, 20),
  timer:sleep(200).
