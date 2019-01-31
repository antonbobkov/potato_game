-module(pop_verifier_json_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

json_get(Key, Map) when is_atom(Key) ->
    maps:get(atom_to_binary(Key, utf8), Map);

json_get(Key, Map) when is_list(Key) ->
    maps:get(list_to_binary(Key), Map).

make_verifier_array(JsonConf) ->
    ChainId = json_get(chain_id, JsonConf),
    JsonVerifierConf = json_get(verifiers, JsonConf),

    VerFunc = 
	fun(Index, VerConf) -> 
		PublicKeyFile = json_get(public_key, VerConf),

		Ip = binary_to_list(json_get(ip, VerConf)),
		Port = json_get(port, VerConf),

		NetAddress = {Ip, Port},

		#verifier_public_info{
		   index = Index, 
		   public_key = my_crypto:read_file_key(public, PublicKeyFile),
		   network_data = {NetAddress, {ChainId, verifier, Index}}
		  } 
	end,

    VerifierArr = array:map(VerFunc, array:from_list(JsonVerifierConf)),

    VerifierArr.

start_pv(VerifierArr, JsonConf, MyIndex, UdpServerId) ->

    MyAddress = (array:get(MyIndex, VerifierArr))#verifier_public_info.network_data,

    {_NetworkAddress, MyNodeId} = MyAddress,

    JsonVerifierConf = json_get(verifiers, JsonConf),
    MyConf = array:get(MyIndex, array:from_list(JsonVerifierConf)),
    PrivateKeyFile = json_get(private_key, MyConf),
    PrivateKey = my_crypto:read_file_key(private, PrivateKeyFile),


    PopChainConfig = #pop_config_data{
		   time_between_blocks = json_get(time_between_blocks_sec, JsonConf), 
		   time_desync_margin = json_get(timestamp_tolerable_error_sec, JsonConf), 
		   chain_id = json_get(chain_id, JsonConf), 
		   verifiers_arr = VerifierArr, 
		   init_time = json_get(genesis_block_timestamp_sec, JsonConf)
		  },

    PopManagerConfig = #pop_manager_config{
			  request_range_backup = json_get(block_request_range_size, JsonConf),

			  net_multi_send = fun(DestAddressList, MsgId, Data) -> 
				             gen_server:cast({global, UdpServerId}, {send, DestAddressList, {MyAddress, MsgId, Data} })
					   end,

			  on_new_block = undefined
			 },

    PopVerifierConfig = #pop_verifier_config{
			   sub_time_out = json_get(subscriber_time_out_sec, JsonConf),
			   my_index = MyIndex,
			   my_key = PrivateKey
			  },

    VerifierPid = spawn_link(fun() -> 
				     pop_verifier:start_loop( 
				       PopChainConfig, 
				       PopManagerConfig, 
				       PopVerifierConfig, 
				       no_timer, 
				       fun () -> ok end
				      )
			     end),

    gen_server:cast({global, UdpServerId}, {add_node, MyNodeId, VerifierPid}),

    VerifierPid.

fun(DestAddressList, MsgId, Data) -> 
	gen_server:cast({global, UdpServerId}, {send, DestAddressList, {MyAddress, MsgId, Data} })
end,

start_server_cluster(VerifierArr, JsonConf, ServerAddress, UdpServerId, ForwardFn) ->
    {_Ip, Port} = ServerAddress,

    %% ?debugVal(ServerAddress),
    %% ?debugVal(UdpServerId),

    gen_server:start_link({global, UdpServerId}, potato_udp, {Port, ForwardFn}, []),

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
			start_pv(VerifierArr, JsonConf, VerIndex, UdpServerId)
		end, 
		IndexList),
    PidList.

start_from_json(JsonConf, ForwardFn) ->
    VerifierArr = make_verifier_array(JsonConf),

    VerAddressList = lists:map(
		       fun(VerData) ->
			       {VerAddress, _} = VerData#verifier_public_info.network_data,
			       VerAddress
		       end,
		       array:to_list(VerifierArr)),

    VerAddressUniqueList = sets:to_list(sets:from_list(VerAddressList)),

    ProcessData = lists:map(
		    fun(Address) ->
			    UdpServerId = {udp_server, Address},
			    PidList = start_server_cluster(VerifierArr, JsonConf, Address, UdpServerId, ForwardFn),
			    {UdpServerId, PidList}
		    end, 
		    VerAddressUniqueList),

    {UdpIdList, VerifierPidList} = lists:unzip(ProcessData),

    %% ?debugVal(UdpIdList),
    %% ?debugVal(VerifierPidList),
    %% ?debugVal(lists:flatten(VerifierPidList)),

    {UdpIdList, lists:flatten(VerifierPidList)}.
    
stop_all({UdpIdList, VerifierPidList}) ->
    lists:foreach(
      fun(UdpId) ->
	      gen_server:stop({global, UdpId})
      end,
      UdpIdList),

    lists:foreach(
      fun(Pid) ->
	      Pid ! exit
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

    broadcast(VerifierPidList, {timer_custom, 110}),

    wait_for_message(send, 1),
    wait_for_message(optimized_send, 3),
    wait_for_message(udp, 3),

    stop_all(Data),
    wait_for_message(terminate, 3),

    ok.
