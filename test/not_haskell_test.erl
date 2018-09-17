-module(not_haskell_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").


mapAccumL_test() ->
  StartList = [1,1,1,1,1,1,1,1],
  EndList = [1,2,3,4,5,6,7,8],
  {Acc, List} = not_haskell:mapAccumL(fun(_x, _acc) -> {_x+_acc, _x+_acc} end, 0, StartList),
  ?assertEqual(Acc, 8),
  ?assertEqual(List, EndList),
  ok.

for_test_recfun(Pid, 0) ->
  Pid ! done,
  receive
    _ -> ?assert(false)
  after
    100 -> ok
  end;
for_test_recfun(Pid, N) ->
  ?debugFmt("receiving ~p~n",[N]),
  receive
    _ -> for_test_recfun(Pid, N-1)
  end.

for_test() ->
  N = 10,
  SelfPid = self(),
  Pid = spawn(fun() -> for_test_recfun(SelfPid, N) end),
  not_haskell:for(fun() -> Pid ! hi end, N),
  receive
    _ -> ok
  end.
