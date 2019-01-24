-module(pop_verifier_json_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

json_get(Key, Map) when is_atom(Key) ->
    maps:get(atom_to_binary(Key), Map);

json_get(Key, Map) when is_list(Key) ->
    maps:get(list_to_binary(Key), Map).

make_verifier_array(JsonVerifierConf, ChainId) ->

    VerFunc = fun(Index, VerConf) -> 
		      PublicKeyFile = json_get(public_key, VerConf),

		      Ip = json_get(ip, VerConf),
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

start_pv(VerifierArr, JsonConfig, MyIndex, UdpServerId) ->

    MyAddress = (array:get(MyIndex, VerifierArr))#verifier_public_info.network_data,

    {_NetworkAddress, MyNodeId} = MyAddress,

    PrivateKey = my_crypto:read_file_key(private, PrivateKeyFile),


    PopChainConfig = #pop_config_data{
		   time_between_blocks = json_get(time_between_blocks_sec, JsonConfig), 
		   time_desync_margin = json_get(timestamp_tolerable_error_sec, JsonConfig), 
		   chain_id = json_get(chain_id, JsonConfig), 
		   verifiers_arr = VerifierArr, 
		   init_time = json_get(genesis_block_timestamp_sec, JsonConfig)
		  },

    PopManagerConfig = #pop_manager_config{
			  request_range_backup = json_get(block_request_range_size, JsonConfig),

			  net_multi_send = fun(DestAddressList, MsgId, Data) -> 
				             gen_server:cast(UdpServerId, {send, DestAddressList, {MyAddress, MsgId, Data} })
					   end,

			  on_new_block = undefined
			 },

    PopVerifierConfig = #pop_verifier_config{
			   sub_time_out = json_get(subscriber_time_out_sec, JsonConfig),
			   my_index = VerifierIndex,
			   my_key = MyKey
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

    gen_server:cast(UdpServerId, {add_node, MyNodeId, VerifierPid}),

    VerifierPid.


verifier_start_stop_test() ->
    Port = 3143,
    NetAddress = {"localhost", Port},

    %% MyPid = self(),
    %% ForwardFn = fun(Code, Data) -> MyPid ! {Code, Data} end,

    ForwardFn = fun(_, _) -> ok end,

    gen_server:start_link({local, potato_udp_name}, potato_udp, {Port, ForwardFn}, []),

    {VerifierArr, PrivateKey, _PublicKey} = make_verifier_array(5, NetAddress),

    VerifierPid = start_pv(VerifierArr, PrivateKey, 0, potato_udp_name),

    VerifierPid ! exit,

    gen_server:stop(potato_udp_name),

    ok.

udp_ver_start(NetAddress = {_IP, Port}, UdpServerName, ForwardFn, VerTot) ->

    gen_server:start_link({local, UdpServerName}, potato_udp, {Port, ForwardFn}, []),

    {VerifierArr, PrivateKey, _PublicKey} = make_verifier_array(VerTot, NetAddress),

    VerPidList = lists:map(fun(VerNum) -> 
				   start_pv(VerifierArr, PrivateKey, VerNum, UdpServerName)
			   end, lists:seq(0, VerTot - 1)),
    
    {VerPidList, UdpServerName}.
    
udp_ver_stop({VerPidList, UdpServerName}) ->

    lists:foreach(fun(VerPid) ->
			  VerPid ! exit
		  end, VerPidList),

    gen_server:stop(UdpServerName),
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

    Port = 3144,
    NetAddress = {"localhost", Port},

    MyPid = self(),
    ForwardFn = fun(Code, _Data) -> 
			%% ?debugFmt("~p ~p ~n", [Code, Data])
			MyPid ! Code 
		end,

    %% ForwardFn = fun(_, _) -> ok end,

    {VerPidList, _} = StateData = udp_ver_start(NetAddress, one_tick_udp_server, ForwardFn, 5),

    wait_for_message(start),
    wait_for_message(add_node, 5),

    broadcast(VerPidList, {timer_custom, 110}),

    wait_for_message(send),
    wait_for_message(optimized_send),
    wait_for_message(udp),

    broadcast(VerPidList, {timer_custom, 120}),

    wait_for_message(send),
    wait_for_message(optimized_send),
    wait_for_message(udp),

    udp_ver_stop(StateData),

    wait_for_message(terminate),

    ok.
    
