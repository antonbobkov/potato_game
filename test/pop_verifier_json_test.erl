-module(pop_verifier_json_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

start_pv(VerifierArr, JsonConf, MyIndex, UdpServerGlobalName, GlobalClusterName) ->

    MyAddress = (array:get(MyIndex, VerifierArr))#verifier_public_info.network_data,

    {_NetworkAddress, MyNodeId} = MyAddress,

    NetSendFn = fun(DestAddressList, MsgId, Data) -> 
			gen_server:cast({global, UdpServerGlobalName}, {send, DestAddressList, {MyAddress, MsgId, Data} })
		end,

    EventFn = fun(_,_) -> ok end,
    ConfPrivateKey = default,

    ConfigData = {MyIndex, NetSendFn, EventFn, ConfPrivateKey},

    VerifierGlobalName = {GlobalClusterName, verifier, MyIndex},

    {ok, VerifierPid} = gen_server:start_link({global, VerifierGlobalName}, pop_verifier, {json_map, JsonConf, ConfigData}, []),

    gen_server:cast({global, UdpServerGlobalName}, {add_node, MyNodeId, VerifierPid}),

    VerifierPid.

start_server_cluster(VerifierArr, JsonConf, ServerAddress, UdpServerGlobalName, ForwardFn) ->
    {_Ip, Port} = ServerAddress,

    {ok, _} = gen_server:start_link({global, UdpServerGlobalName}, potato_udp, {Port, ForwardFn}, []),

    VerList = lists:filter(
		fun(VerData) ->
			{VerAddress, _} = VerData#verifier_public_info.network_data,
			VerAddress == ServerAddress
		end,
		array:to_list(VerifierArr)),

    IndexList = lists:map(
		  fun(VerData) ->
			  VerData#verifier_public_info.index
		  end,
		  VerList),

    PidList = lists:map(
		fun(VerIndex) ->
			start_pv(VerifierArr, JsonConf, VerIndex, UdpServerGlobalName)
		end, 
		IndexList),
    PidList.

start_from_json(JsonConf, ForwardFn, GlobalClusterName) ->
    VerifierArr = pop_verifier:make_verifier_array_from_json(JsonConf),

    VerAddressList = lists:map(
		       fun(VerData) ->
			       {VerAddress, _} = VerData#verifier_public_info.network_data,
			       VerAddress
		       end,
		       array:to_list(VerifierArr)),

    VerAddressUniqueList = sets:to_list(sets:from_list(VerAddressList)),

    ProcessData = lists:map(
		    fun(Address) ->
			    UdpServerGlobalName = {GlobalClusterName, udp_server, Address},
			    PidList = start_server_cluster(VerifierArr, JsonConf, Address, UdpServerGlobalName, ForwardFn),
			    {UdpServerGlobalName, PidList}
		    end, 
		    VerAddressUniqueList),

    {UdpIdList, VerifierPidList} = lists:unzip(ProcessData),

    {UdpIdList, lists:flatten(VerifierPidList)}.
    
stop_all({UdpIdList, VerifierPidList}) ->
    lists:foreach(
      fun(UdpId) ->
	      gen_server:stop({global, UdpId})
      end,
      UdpIdList),

    lists:foreach(
      fun(Pid) ->
	      gen_server:stop(Pid)
      end,
      VerifierPidList),

    ok.

start_stop_test() ->
    JsonFileName = "test/test_config_3.json",

    {ok, FileData} = file:read_file(JsonFileName),

    JsonConf = jsx:decode(FileData, [return_maps]),

    ForwardFn = fun(_, _) -> ok end,

    Data = start_from_json(JsonConf, ForwardFn),

    stop_all(Data),

    ok.
    


broadcast(VerPidList, Msg) ->

    lists:foreach(fun(VerPid) ->
			  VerPid ! Msg
		  end, VerPidList).

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
    ForwardFn = fun(Code, _Data) -> 
			%% ?debugFmt("~p ~n ~p ~n", [Code, Data]),
			MyPid ! Code 
		end,

    %% ForwardFn = fun(_, _) -> ok end,

    {_UdpIdList, VerifierPidList} = Data = start_from_json(JsonConf, ForwardFn),

    wait_for_message(start, 3),
    wait_for_message(add_node, 4),

    broadcast(VerifierPidList, {custom_timer_tick, 110}),

    wait_for_message(send, 1),
    wait_for_message(optimized_send, 3),
    wait_for_message(udp, 3),

    stop_all(Data),
    wait_for_message(terminate, 3),

    ok.
