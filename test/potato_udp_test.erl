-module(potato_udp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

game_loop() ->
  receive
    _Any -> ok
  end.

potato_udp_test() ->
  Port = 3142,
  {ok, Pid} = gen_server:start(potato_udp, Port, []),
  %%?debugFmt("started potato_udp gen_server ~p~n",[Pid]),

  SomeGame = spawn(fun game_loop/0),
  SomeVerifierId = [0,0],
  SomeGameId = 0,
  gen_server:cast(Pid, {add_game, {SomeGameId, SomeVerifierId}, SomeGame}),

  %% send a valid message
  gen_server:cast(Pid, {send, {"localhost", Port}, typetato:pack_unsigned({game_id, SomeGameId},"hi")}),
  %% send an invalid message
  gen_server:cast(Pid, {send, {"localhost", Port}, "hi"}),
  ok = gen_server:stop(Pid).

  %% TODO better tests
