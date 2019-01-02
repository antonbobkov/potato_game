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

    gen_server:cast(Pid, {add_node, my_id, SomeGame}),

    %% send a valid message

    GameAddress = {{"localhost", Port}, my_id},

    gen_server:cast(Pid, {send, GameAddress, "hi"}),

    %% %% send an invalid message
    %% gen_server:cast(Pid, {send, {"localhost", Port}, "hi"}),
    ok = gen_server:stop(Pid).

%% TODO better tests
