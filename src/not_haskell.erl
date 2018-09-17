-module(not_haskell).

-export([mapAccumL/3, for/2]).


-spec mapAccumL(Fun, Acc, List) -> {Acc, List} when
  %% boo args are backwards... why...
  Fun :: fun((Elem :: T, Acc) -> {Acc, Out}),
  Acc :: term(),
  List :: [T],
  Out :: term(),
  T :: term().
mapAccumL(Fun, Acc, List) ->
  lists:foldl(fun(_x, {_acc, _outlist}) ->
    {NAcc, NOut} = Fun(_x, _acc),
    %% inefficient snocing, w/e
    %% TODO implement using foldr instead
    {NAcc, _outlist ++ [NOut]}
  end, {Acc,[]}, List).

%% not exactly forM but whatever
%% probably actually is some haskell function that does this but I can't remeber what it is anymore
-spec for(Fun, N) -> no_return() when
  Fun :: fun(() -> no_return()),
  N :: integer().
for(_, 0) ->
  ok;
for(Fun, N) ->
  Fun(),
  for(Fun, N-1).
