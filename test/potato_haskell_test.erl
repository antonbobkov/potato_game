-module(potato_haskell_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").


mapAccumL_test() ->
  StartList = [1,1,1,1,1,1,1,1],
  EndList = [1,2,3,4,5,6,7,8],
  {OutAcc, OutList} = potato_haskell:mapAccumL(fun(Acc, X) -> {X+Acc, X+Acc} end, 0, StartList),
  ?assertEqual(OutAcc, 8),
  ?assertEqual(OutList, EndList),
  ok.

for_test_recfun(Pid, 0) ->
  Pid ! done,
  receive
    _ -> ?assert(false)
  after
    100 -> ok
  end;
for_test_recfun(Pid, N) ->
  receive
    _ -> for_test_recfun(Pid, N-1)
  end.

for_test() ->
  N = 10,
  SelfPid = self(),
  Pid = spawn(fun() -> for_test_recfun(SelfPid, N) end),
  potato_haskell:for(fun() -> Pid ! hi end, N),
  receive
    _ -> ok
  end.

unfoldr_test() ->
  UnfoldFn = fun(X) ->
    if X < 10 -> {X, X+1};
      true -> imdone
    end
  end,

  Out = potato_haskell:unfoldr(UnfoldFn, 0),
  ?assertEqual(Out, [0,1,2,3,4,5,6,7,8,9]).
