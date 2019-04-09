-module(potato_cluster).

-export([
	 start_one_pop_verifier/5,
	 start_single_server_cluster/5,
	 start_cluster_from_json/2,
	 stop_cluster/1,
	 start_web_cluster/1
	]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("potato_records.hrl").

json_find(Key, Map) when is_atom(Key) ->
    maps:find(atom_to_binary(Key, utf8), Map).

json_get(Key, Map) when is_atom(Key) ->
    maps:get(atom_to_binary(Key, utf8), Map).

json_get_str(Key, Map) when is_atom(Key) ->
    binary_to_list(maps:get(atom_to_binary(Key, utf8), Map)).

json_put(Key, Value, Map) when is_atom(Key) ->
    maps:put(atom_to_binary(Key, utf8), Value, Map).

start_one_pop_verifier(VerifierArr, JsonConf, MyIndex, UdpServerId, OnEventFn) ->

    MyAddress = (array:get(MyIndex, VerifierArr))#verifier_public_info.network_data,

    {_NetworkAddress, MyNodeNetId} = MyAddress,

    NetSendFn = fun(DestAddressList, MsgId, Data) -> 
			gen_server:cast({global, UdpServerId}, {send, DestAddressList, {MyAddress, MsgId, Data} })
		end,

    ConfPrivateKey = default,

    VerifierOnEventFn = fun(Code, Data) -> OnEventFn(MyNodeNetId, Code, Data) end,

    ConfigData = {MyIndex, NetSendFn, VerifierOnEventFn, ConfPrivateKey},

    gen_server:start_link({global, MyNodeNetId}, pop_verifier, {json_map, JsonConf, ConfigData}, []),

    gen_server:cast({global, UdpServerId}, {add_node, MyNodeNetId, {global, MyNodeNetId}}),

    MyNodeNetId.

start_single_server_cluster(VerifierArr, JsonConf, ServerAddress, UdpServerId, OnEventFn) ->
    {_Ip, Port} = ServerAddress,

    UdpOnEventFn = fun(Code, Data) -> OnEventFn(UdpServerId, Code, Data) end,

    gen_server:start_link({global, UdpServerId}, potato_udp, {Port, UdpOnEventFn}, []),

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
			start_one_pop_verifier(VerifierArr, JsonConf, VerIndex, UdpServerId, OnEventFn)
		end, 
		IndexList),
    IdList.


start_cluster_from_json(JsonConf_0, OnEventFn) ->

    JsonConf = 
	case json_get(genesis_block_timestamp_sec, JsonConf_0) of

	    <<"now">> ->
		Time = erlang:system_time(second),
		json_put(genesis_block_timestamp_sec, Time, JsonConf_0);

	    _ ->
		JsonConf_0
	end,

    
    
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
			    IdList = start_single_server_cluster(VerifierArr, JsonConf, Address, UdpServerId, OnEventFn),
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


format_message(full, Source, Code, Data) ->
    io_lib:format("~p ~p ~n ~p ~n ~n", [Source, Code, Data]);

format_message(code, Source, Code, _Data) ->
    io_lib:format("~p ~n ~p ~n ~n", [Source, Code]).

block_chain_output(Source, Code = new_block_created, Block, BlockLogFile) ->
    file:write_file(BlockLogFile, format_message(full, Source, Code, Block), [append]);

block_chain_output(_, _, _, _) ->
    ok.


start_web_cluster(Args) ->
    try
	start_web_cluster_inner(Args)
    catch
	_:Err ->
	    Stacktrace = erlang:get_stacktrace(),
	    ?debugFmt("~p ~n ~p ~n", [Err, Stacktrace]),
	    erlang:error("can't start")
    end.

 
start_web_cluster_inner([JsonFileName, LogModeStr]) ->

    %% ?debugFmt("~p ~n ~p ~n", [JsonFileName, LogModeStr]),

    LogMode = LogModeStr,

    {ok, FileData} = file:read_file(JsonFileName),

    JsonConf = jsx:decode(FileData, [return_maps]),

    case json_find(web, JsonConf) of
	{ok, <<"none">>} ->
	    no_web;
	{ok, JsonWebConf} ->

	    WebPort = json_get(port, JsonWebConf),
	    WebLogsDir = json_get_str(logs_dir, JsonWebConf),
	    WebFileDir = json_get_str(file_dir, JsonWebConf),

	    potato_cluster_web_server:start(WebPort, WebLogsDir, WebFileDir),
	    ok;
	error ->
	    no_web
    end,

    OnEventFn = 
	case LogMode of 

	    no_logs ->
		fun(_, _, _) -> ok end;

	    console_logs ->
		fun(Source, Code, Data) -> io:format(format_message(full, Source, Code, Data)) end;

	    file_logs_full ->
		LogFile = json_get(log_file, JsonConf),
		file:write_file(LogFile, ""),

		BlockLogFile = json_get(block_log_file, JsonConf),
		file:write_file(BlockLogFile, ""),

		fun(Source, Code, Data) -> 
			file:write_file(LogFile, format_message(full, Source, Code, Data), [append]),
			block_chain_output(Source, Code, Data, BlockLogFile),
			ok
		end;

	    file_logs_codes ->
		LogFile = json_get(log_file, JsonConf),
		file:write_file(LogFile, ""),

		BlockLogFile = json_get(block_log_file, JsonConf),
		file:write_file(BlockLogFile, ""),

		fun(Source, Code, Data) -> 
			file:write_file(LogFile, format_message(code, Source, Code, Data), [append]),
			block_chain_output(Source, Code, Data, BlockLogFile),
			ok
		end
	end,		   

    start_cluster_from_json(JsonConf, OnEventFn),

    ok.
