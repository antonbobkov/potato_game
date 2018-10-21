-module(potato_udp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

potato_udp_test() ->
  Port = 3142,
  {ok, Pid} = gen_server:start(potato_udp, Port, []),
  %% ?debugFmt("started potato_udp gen_server ~p~n",[Pid]),
  %% send a valid message
  gen_server:cast(Pid, {send, {"localhost", Port}, messages:pack_unsigned(0,"hi")}),
  %% send an invalid message
  gen_server:cast(Pid, {send, {"localhost", Port}, "hi"}),
  ok = gen_server:stop(Pid).
  %% TODO better tests
