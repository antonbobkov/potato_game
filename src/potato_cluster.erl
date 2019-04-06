-module(potato_cluster).

-export([
	 hi/0,
	 start_one_pop_verifier/4,
	 start_single_server_cluster/5,
	 start_cluster_from_json/2,
	 stop_cluster/1,
	 start_web_cluster/1
	]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("potato_records.hrl").

hi() ->
    ?debugFmt("hi", []),
    ok.

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

json_find(Key, Map) when is_atom(Key) ->
    maps:find(atom_to_binary(Key, utf8), Map).

json_get(Key, Map) when is_atom(Key) ->
    maps:get(atom_to_binary(Key, utf8), Map).

format_message(Code, _Data) ->
    io_lib:format("~p ~n", [Code]).

start_web_cluster([JsonFileName, LogModeStr]) ->

    ?debugFmt("~p ~n ~p ~n", [JsonFileName, LogModeStr]),

    LogMode = LogModeStr,

    {ok, FileData} = file:read_file(JsonFileName),

    JsonConf = jsx:decode(FileData, [return_maps]),

    LogFile = 
	case json_find(log_file, JsonConf) of
	    {ok, <<"none">>} ->
		no_log_file;
	    {ok, FileName} ->
		FileName;
	    error ->
		no_log_file
	end,

    case json_find(web, JsonConf) of
	{ok, <<"none">>} ->
	    no_web;
	{ok, JsonWebConf} ->
	    WebPort = json_get(web_port, JsonWebConf),
	    potato_cluster_web_server:start(WebPort),
	    ok;
	error ->
	    no_web
    end,

    ForwardFn = 
	case LogMode of 
	    no_logs ->
		fun(_, _) -> ok end;
	    console_logs ->
		fun(Code, Data) -> io:format(format_message(Code, Data)) end;
	    file_logs ->
		fun(Code, Data) -> 
			?assertNotEqual(LogFile, no_log_file),
			file:write_file(LogFile, format_message(Code, Data), [append])
		end
	end,		   

    start_cluster_from_json(JsonConf, ForwardFn),

    ok.

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
    
stop_cluster({UdpServerIdList, VerifierIdList}) ->
    lists:foreach(
      fun(UdpId) ->
	      gen_server:stop({global, UdpId})
      end,
      UdpServerIdList),

    lists:foreach(
      fun(Id) ->
	      gen_server:stop({global, Id})
      end,
      VerifierIdList),

    ok.
