-module(potato_haskell).

-export([mapAccumL/3, for/2, unfoldr/2]).


-spec mapAccumL(Fun, Acc, List) -> {Acc, List} when
  Fun :: fun((Acc, Elem :: T) -> {Acc, Out}),
  Acc :: term(),
  List :: [T],
  Out :: term(),
  T :: term().
mapAccumL(Fun, Acc, List) ->
  lists:foldl(fun(X, {FAcc, Outlist}) ->
    {NAcc, NOut} = Fun(FAcc, X),
    %% inefficient snocing, w/e
    %% TODO implement using foldr instead
    {NAcc, Outlist ++ [NOut]}
  end, {Acc,[]}, List).

-spec unfoldr(Fun, Acc) -> List when
  Fun :: fun((Acc) -> Out),
  Out :: {Elem :: T, Acc} | term(),
  List :: [T].
unfoldr(Fun, Acc) -> lists:reverse(unfoldr_internal(Fun, Acc, [])).

unfoldr_internal(Fun, Acc, Out) ->
  case Fun(Acc) of
    {X, NAcc} -> unfoldr_internal(Fun, NAcc, [X]++Out);
    _ -> Out
  end.

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
