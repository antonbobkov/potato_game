-module(potato_udp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% game_loop() ->
%%     receive
%% 	_Any -> ok
%%     end.

wait_for_event(Code) ->
    receive {Code, _} ->
	    ok
    after 100 ->
	    erlang:error(timeout, Code)
    end.
    

start_stop_test() ->
    Port = 3142,
    MyPid = self(),

    ForwardFn = fun(Code, Data) -> MyPid ! {Code, Data} end,

    gen_server:start_link({local, potato_udp_name}, potato_udp, {Port, ForwardFn}, []),

    wait_for_event(start),

    gen_server:stop(potato_udp_name),

    wait_for_event(terminate),

    ok.

one_message_test() ->
    Port = 3143,
    MyPid = self(),

    ForwardFn = fun(Code, Data) -> MyPid ! {Code, Data} end,

    gen_server:start_link({local, potato_udp_name}, potato_udp, {Port, ForwardFn}, []),

    gen_server:cast(potato_udp_name, {add_node, main_node, MyPid}),

    NodeAddress = {{"localhost", Port}, main_node},

    gen_server:cast(potato_udp_name, {send, [NodeAddress], "hi"}),

    wait_for_event(net),

    gen_server:stop(potato_udp_name),

    wait_for_event(terminate),

    ok.
    


%% potato_udp_test() ->
%%     Port = 3142,
%%     {ok, Pid} = gen_server:start(potato_udp, Port, []),
%%     %%?debugFmt("started potato_udp gen_server ~p~n",[Pid]),

%%     SomeGame = spawn(fun game_loop/0),

%%     gen_server:cast(Pid, {add_node, my_id, SomeGame}),

%%     %% send a valid message

%%     GameAddress = {{"localhost", Port}, my_id},

%%     gen_server:cast(Pid, {send, [GameAddress], "hi"}),

%%     timer:sleep(100),

%%     %% %% send an invalid message
%%     %% gen_server:cast(Pid, {send, {"localhost", Port}, "hi"}),
%%     ok = gen_server:stop(Pid).

%% TODO better tests
