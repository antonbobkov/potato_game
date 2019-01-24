-module(pop_verifier_udp_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

make_verifier_array(VerNum, NetAddress) ->
    %% make verifier array, they share keys

    PrivateKey = my_crypto:read_file_key(private, "key1.prv"),
    PublicKey = my_crypto:read_file_key(public, "key1.pub"),

    VerFunc = fun(Index) -> 
		      #verifier_public_info{
			 index = Index, 
			 public_key = PublicKey,
			 network_data = {NetAddress, {verifier, Index}}
			} 
	      end,
    VerifierArr = array:from_list(lists:map(VerFunc, lists:seq(0, VerNum - 1))),

    {VerifierArr, PrivateKey, PublicKey}.

start_pv(VerifierArr, PrivateKey, VerifierIndex, UdpServerId) ->

    CurrentTime = 100,

    MyAddress = (array:get(VerifierIndex, VerifierArr))#verifier_public_info.network_data,

    {_NetworkAddress, MyNodeId} = MyAddress,


    PopChainConfig = #pop_config_data{
		   time_between_blocks = 10, 
		   time_desync_margin = 5, 
		   chain_id = hype_chain, 
		   verifiers_arr = VerifierArr, 
		   init_time = CurrentTime
		  },

    PopManagerConfig = #pop_manager_config{
			  request_range_backup = 3,

			  net_multi_send = fun(DestAddressList, MsgId, Data) -> 
				             gen_server:cast(UdpServerId, {send, DestAddressList, {MyAddress, MsgId, Data} })
					   end,

			  on_new_block = undefined
			 },

    PopVerifierConfig = #pop_verifier_config{
			   sub_time_out = 20,
			   my_index = VerifierIndex,
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
    
