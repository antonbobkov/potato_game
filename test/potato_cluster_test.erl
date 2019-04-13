-module(potato_cluster_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").


start_stop_test() ->
    JsonFileName = "test/test_config_3.json",

    {ok, FileData} = file:read_file(JsonFileName),

    JsonConf = jsx:decode(FileData, [return_maps]),

    ForwardFn = fun(_, _, _) -> ok end,

    Data = potato_cluster:start_cluster_from_json(JsonConf, ForwardFn),

    potato_cluster:stop_cluster(Data),

    ok.
    


broadcast(VerIdList, Msg) ->

    lists:foreach(fun(VerId) ->
			  gen_server:cast({global, VerId}, Msg)
		  end, VerIdList).

wait_for_message(Code) ->
    receive 
	Code ->
	    ok
    after 100 ->
	    erlang:error(timeout)
    end.

wait_for_message(Code, Count) ->
    lists:foreach(fun(_) -> wait_for_message(Code) end, lists:seq(1, Count)).

one_udp_tick_test() ->

    JsonFileName = "test/test_config_1_1_2.json",

    {ok, FileData} = file:read_file(JsonFileName),

    JsonConf = jsx:decode(FileData, [return_maps]),

    MyPid = self(),
    ForwardFn = fun(_, Code, _Data) -> 
			%% ?debugFmt("~p ~n ~p ~n", [Code, Data]),
			MyPid ! Code 
		end,

    %% ForwardFn = fun(_, _) -> ok end,

    {_UdpIdList, VerifierIdList} = Data = 
	potato_cluster:start_cluster_from_json(JsonConf, ForwardFn),

    wait_for_message(start, 3),
    wait_for_message(add_node, 4),

    broadcast(VerifierIdList, {custom_timer_tick, 110}),

    wait_for_message(send, 1),
    wait_for_message(optimized_send, 3),
    wait_for_message(udp, 3),

    potato_cluster:stop_cluster(Data),
    wait_for_message(terminate, 3),

    ok.
