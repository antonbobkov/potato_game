-module(potato_cluster).

-export([
	 start_one_pop_verifier/4,
	 start_single_server_cluster/5,
	 start_cluster_from_json/2,
	 stop_cluster/1
	]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("potato_records.hrl").

start_one_pop_verifier(VerifierArr, JsonConf, MyIndex, UdpServerId) ->

    MyAddress = (array:get(MyIndex, VerifierArr))#verifier_public_info.network_data,

    {_NetworkAddress, MyNodeNetId} = MyAddress,

    NetSendFn = fun(DestAddressList, MsgId, Data) -> 
			gen_server:cast({global, UdpServerId}, {send, DestAddressList, {MyAddress, MsgId, Data} })
		end,

    EventFn = fun(_,_) -> ok end,
    ConfPrivateKey = default,

    ConfigData = {MyIndex, NetSendFn, EventFn, ConfPrivateKey},

    gen_server:start_link({global, MyNodeNetId}, pop_verifier, {json_map, JsonConf, ConfigData}, []),

    gen_server:cast({global, UdpServerId}, {add_node, MyNodeNetId, {global, MyNodeNetId}}),

    MyNodeNetId.

start_single_server_cluster(VerifierArr, JsonConf, ServerAddress, UdpServerId, ForwardFn) ->
    {_Ip, Port} = ServerAddress,

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

    IdList = lists:map(
		fun(VerIndex) ->
			start_one_pop_verifier(VerifierArr, JsonConf, VerIndex, UdpServerId)
		end, 
		IndexList),
    IdList.

start_cluster_from_json(JsonConf, ForwardFn) ->
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
			    UdpServerId = {udp_server, Address},
			    IdList = start_single_server_cluster(VerifierArr, JsonConf, Address, UdpServerId, ForwardFn),
			    {UdpServerId, IdList}
		    end, 
		    VerAddressUniqueList),

    {UdpServerIdList, VerifierIdList} = lists:unzip(ProcessData),

    {UdpServerIdList, lists:flatten(VerifierIdList)}.
    
stop_cluster({UdpIdList, VerifierIdList}) ->
    lists:foreach(
      fun(UdpId) ->
	      gen_server:stop({global, UdpId})
      end,
      UdpIdList),

    lists:foreach(
      fun(Id) ->
	      gen_server:stop({global, Id})
      end,
      VerifierIdList),

    ok.
