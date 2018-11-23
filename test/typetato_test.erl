-module(typetato_test).

-include_lib("eunit/include/eunit.hrl").


typetato_test() ->
  %% success matchings
  Test1 = typetato:pack_unsigned({game_id, 0},"hi"),
  ?assertEqual({ok, {{game_id, 0},"hi"}}, typetato:unpack(Test1)),

  %% TODO why does using this timeout
  %%{SomeKey,_} = my_crypto:potato_key(),
  SomeKey = [0,0],
  Test2 = typetato:pack_unsigned({targeted, {0, SomeKey}},"hi"),
  ?assertEqual({ok, {{targeted, {0, SomeKey}},"hi"}}, typetato:unpack(Test2)),

  Test3 = typetato:pack_unsigned(something_invalid,"hi"),
  ?assertEqual(fail, typetato:unpack(Test3)).
