-module(web3_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

web3_test() ->
  {ok, Pid} = gen_server:start_link(web3, [], []),
  %% ?debugFmt("~p~n",[Pid]),
  N1 = gen_server:call(Pid, []),
  ?assertEqual(-1, N1),

  % give time for server to update
  timer:sleep(200),
  N2 = gen_server:call(Pid, []),
  ?assertEqual(113, N2),

  gen_server:cast(Pid, stop).
