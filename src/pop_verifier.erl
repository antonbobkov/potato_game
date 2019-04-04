-module(pop_verifier).
-behavior(gen_server).

-export([
	 init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2,
	 make_verifier_array_from_json/1
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

make_event(Code, Data, State) ->
    EventFn = State#pop_verifier.config#pop_verifier_config.event_fn,
    EventFn(Code, Data),
    ok.

subscriber_clean_up(CurrentTime, PopVerifier) ->
    Timeout = PopVerifier#pop_verifier.config#pop_verifier_config.sub_time_out,

    FilterFn = fun(_, Time) -> Time + Timeout > CurrentTime end,
    
    Subs0 = PopVerifier#pop_verifier.subscribers,

    Subs1 = maps:filter(FilterFn, Subs0),
    
    PopVerifier#pop_verifier{subscribers = Subs1}.

emit_net_message(verifiers, MsdId, Data, PopVerifier) ->
    VerifiersArr = PopVerifier#pop_verifier.verifiers_arr,
    VerifiersAddressArr = array:map(fun(_, V) -> V#verifier_public_info.network_data end, VerifiersArr),
    emit_net_message_to_list(array:to_list(VerifiersAddressArr), MsdId, Data, PopVerifier);

emit_net_message(subs, MsdId, Data, PopVerifier) ->
    emit_net_message_to_list(maps:keys(PopVerifier#pop_verifier.subscribers), MsdId, Data, PopVerifier).

emit_net_message_to_list(AddressList, MsdId, Data, PopVerifier) ->
    NetMultiSendFn = PopVerifier#pop_verifier.pop_manager#pop_manager.config#pop_manager_config.net_multi_send,

    %% lists:foreach(fun(Address) -> NetSendFn(Address, MsdId, Data) end, AddressList),
    
    NetMultiSendFn(AddressList, MsdId, Data),
    
    ok.

on_timer(CurrentTime, PV0) ->
    PV1 = subscriber_clean_up(CurrentTime, PV0),

    PopChain = PV1#pop_verifier.pop_manager#pop_manager.pop_chain,
    Inx = PV1#pop_verifier.config#pop_verifier_config.my_index,

    GenerationTime = pop_chain:get_verfier_next_block_time(Inx, PopChain),

    if GenerationTime =< CurrentTime ->
	    NewBlockUnsigned = pop_chain:generate_new_block(Inx, PopChain),

	    PrivateKey = PV1#pop_verifier.config#pop_verifier_config.my_key,

	    NewBlock = pop_chain:apply_block_signature(my_crypto:sign(maps:get(this_id, NewBlockUnsigned), PrivateKey), NewBlockUnsigned),

	    emit_net_message(verifiers, send_full_blocks, {new, [NewBlock]}, PV1),

	    ok;

       true -> ok
    end,
    
    PV1.

get_current_time(State) ->
    InitInterval = State#pop_verifier.config#pop_verifier_config.timer_interval,

    CurrentTime = State#pop_verifier.current_time,

    if CurrentTime == undefined ->
	    ?assertNotEqual(none, InitInterval),

	    erlang:system_time(second);
       true ->
	    ?assertEqual(none, InitInterval),

	    CurrentTime
    end.

json_get(Key, Map) when is_atom(Key) ->
    maps:get(atom_to_binary(Key, utf8), Map).

%% json_get(Key, Map) when is_list(Key) ->
%%     maps:get(list_to_binary(Key), Map).

make_verifier_array_from_json(JsonConf) ->
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

create_config_from_json(JsonConf, _ConfigData = {MyIndex, NetSendFn, EventFn, ConfPrivateKey}) ->

    VerifierArr = make_verifier_array_from_json(JsonConf),

    %% MyAddress = (array:get(MyIndex, VerifierArr))#verifier_public_info.network_data,

    %% {_NetworkAddress, MyNodeId} = MyAddress,

    if ConfPrivateKey == default ->
	    JsonVerifierConf = json_get(verifiers, JsonConf),
	    MyConf = array:get(MyIndex, array:from_list(JsonVerifierConf)),
	    PrivateKeyFile = json_get(private_key, MyConf),
	    PrivateKey = my_crypto:read_file_key(private, PrivateKeyFile);
       true ->
	    PrivateKey = ConfPrivateKey
    end,

    TimerInterval = json_get(timer_tick_interval_sec, JsonConf),

    if is_integer(TimerInterval) ->
	    TimerIntervalFinal = TimerInterval;

       TimerInterval == <<"none">> ->
	    TimerIntervalFinal = none;

       true ->
	    TimerIntervalFinal = erlang:error(timer_interval_bad_value, [TimerInterval])
    end,

    PopChainConfig = #pop_config_data{
		   time_between_blocks = json_get(time_between_blocks_sec, JsonConf), 
		   time_desync_margin = json_get(timestamp_tolerable_error_sec, JsonConf), 
		   chain_id = json_get(chain_id, JsonConf), 
		   verifiers_arr = VerifierArr, 
		   init_time = json_get(genesis_block_timestamp_sec, JsonConf)
		  },

    PopManagerConfig = #pop_manager_config{
			  request_range_backup = json_get(block_request_range_size, JsonConf),

			  net_multi_send = NetSendFn,

			  on_new_block = undefined
			 },

    PopVerifierConfig = #pop_verifier_config{
			   sub_time_out = json_get(subscriber_time_out_sec, JsonConf),
			   my_index = MyIndex,
			   my_key = PrivateKey,
			   event_fn = EventFn,
			   timer_interval = TimerIntervalFinal
			  },
    
    {PopChainConfig, PopManagerConfig, PopVerifierConfig}.
    
init({json_file, JsonFilePath, ConfigData}) ->

    {ok, FileData} = file:read_file(JsonFilePath),

    JsonConf = jsx:decode(FileData, [return_maps]),

    init({json_map, JsonConf, ConfigData});

init({json_map, JsonConf, ConfigData}) ->
    {PopConfigData, PopManagerConfig, PopVerConfig} = create_config_from_json(JsonConf, ConfigData),

    init({explicit, PopConfigData, PopManagerConfig, PopVerConfig});
    

init(InitData = {explicit, PopConfigData, PopManagerConfig, PopVerConfig}) ->
    TimerIntervalSec = PopVerConfig#pop_verifier_config.timer_interval,

    ?assertNotEqual(undefined, TimerIntervalSec),
    ?assertNotEqual(undefined, PopVerConfig#pop_verifier_config.event_fn),

    if TimerIntervalSec == none ->
	    CurrentTime = PopConfigData#pop_config_data.init_time,
	    TimerRef = undefined;
       true ->
	    CurrentTime = undefined,
	    {ok, TimerRef} = timer:send_interval(TimerIntervalSec * 1000, real_timer_tick)
    end,

    OnNewBlockFn = fun(Block) -> self() ! {new_block, Block} end,

    ?assertEqual(undefined, PopManagerConfig#pop_manager_config.on_new_block),

    NewPopManagerConfig = PopManagerConfig#pop_manager_config{on_new_block = OnNewBlockFn},

    State = #pop_verifier{
	       subscribers = maps:new(),
	       pop_manager = pop_manager:new(PopConfigData, NewPopManagerConfig),
	       config = PopVerConfig,
	       verifiers_arr = PopConfigData#pop_config_data.verifiers_arr,

	       current_time = CurrentTime,
	       timer_ref = TimerRef
	      },

    make_event(start, InitData, State),

    {ok, State}.

%% @doc Adds/updates a subscriber.
%% 
%% Subscribers are sent updates on when new blocks are added.
%% Each sub expired after a fixed time, to stay subscribed this message needs to be spammed periodically.
%% (Similar to ping)

handle_cast({net_udp, Data = {SenderAddress, subscribe, _} }, State) ->

    make_event(net_udp, Data, State),
 
    CurrentTime = get_current_time(State),

    Subs = State#pop_verifier.subscribers,

    NewState = State#pop_verifier{subscribers = maps:put(SenderAddress, CurrentTime, Subs)},

    {noreply, NewState};

handle_cast({net_udp, Data = {SenderAddress, MsgId, NetData} }, State) ->

    make_event(net_udp, Data, State),

    CurrentTime = get_current_time(State),

    PM0 = State#pop_verifier.pop_manager,

    PM1 = pop_manager:on_net_message(SenderAddress, CurrentTime, MsgId, NetData, PM0),
    
    NewState = State#pop_verifier{pop_manager = PM1},

    {noreply, NewState};

handle_cast({new_block, NewBlock}, State) ->
    make_event(new_block, NewBlock, State),

    Sz = maps:size(State#pop_verifier.subscribers),

    if Sz /= 0 ->
	    emit_net_message(subs, send_full_blocks, {new, NewBlock}, State);

       true -> 
	    ok
    end,
				 
    {noreply, State};

handle_cast(real_timer_tick, State) ->
    ?assertNotEqual(undefined, State#pop_verifier.timer_ref),

    CurrentTime = erlang:system_time(second),

    make_event(real_timer_tick, CurrentTime, State),

    NewState = on_timer(CurrentTime, State),

    {noreply, NewState};

handle_cast(exit, State) ->
    {stop, normal, State};

handle_cast({custom_timer_tick, CurrentTime}, State) ->
    ?assertEqual(undefined, State#pop_verifier.timer_ref),

    make_event(custom_timer_tick, CurrentTime, State),

    State1 = on_timer(CurrentTime, State),

    State2 = State1#pop_verifier{current_time = CurrentTime},

    {noreply, State2};

handle_cast(Data, _State) ->
    erlang:error(unexpected_handle_cast, [Data]).

% Kind of hacky - forward messages as cast calls
handle_info(Data, State) ->
    handle_cast(Data, State).


handle_call(E, From, _S) ->
    erlang:error(unexpected_handle_call, [E, From]).

code_change(OldVsn, _State, Extra) ->
    erlang:error(unexpected_code_change, [OldVsn, Extra]).

terminate(Reason, State) ->

    TimerRef = State#pop_verifier.timer_ref,

    if TimerRef /= undefined ->
	    {ok, cancel} = timer:cancel(TimerRef);
       true -> ok
    end,

    make_event(terminate, Reason, State),

    ok.
