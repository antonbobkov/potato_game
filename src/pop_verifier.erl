%% @doc code that handles running a full verifier node.

-module(pop_verifier).

-export([
	 new/3,
	 on_net_message/5,
	 on_timer/2,
	 on_new_block/2,
	 start_loop/5
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").


%% @doc Create new verifier. 
%% 
%% Needs to hook up PopManagerConfig.on_new_block to pop_verifier:on_new_block

new(PopConfigData, PopManagerConfig, PopVerConfig) ->
    #pop_verifier{
       subscribers = maps:new(),
       pop_manager = pop_manager:new(PopConfigData, PopManagerConfig),
       config = PopVerConfig,
       verifiers_arr = PopConfigData#pop_config_data.verifiers_arr
      }.

%% @doc Adds/updates a subscriber.
%% 
%% Subscribers are sent updates on when new blocks are added.
%% Each sub expired after a fixed time, to stay subscribed this message needs to be spammed periodically.
%% (Similar to ping)

on_net_message(SenderAddress, CurrentTime, subscribe, _, PopVerifier) ->
    Subs = PopVerifier#pop_verifier.subscribers,

    PopVerifier#pop_verifier{subscribers = maps:put(SenderAddress, CurrentTime, Subs)};

on_net_message(SenderAddress, CurrentTime, MsgId, Data, PopVerifier) ->
    PM0 = PopVerifier#pop_verifier.pop_manager,

    PM1 = pop_manager:on_net_message(SenderAddress, CurrentTime, MsgId, Data, PM0),
    
    PopVerifier#pop_verifier{pop_manager = PM1}.


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
    NetSendFn = PopVerifier#pop_verifier.pop_manager#pop_manager.config#pop_manager_config.net_send,

    lists:foreach(fun(Address) -> NetSendFn(Address, MsdId, Data) end, AddressList),
    
    ok.

%% @doc Process time related events: subscriber clean up and block generation

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

%% @doc Sends new block info to subscribers
on_new_block(NewBlock, PopVerifier) ->
    emit_net_message(subs, send_full_blocks, {new, NewBlock}, PopVerifier),
    PopVerifier.

%% @doc starts loop handling the verifier

start_loop(PopConfigData, PopManagerConfig, PopVerConfig, TimerIntervalSec, OnExitFn) ->

    if TimerIntervalSec /= no_timer ->
	    TimerRef = timer:send_interval(TimerIntervalSec * 1000, timer_real),
	    Data = {TimerRef, OnExitFn};
       true ->
	    Data = {no_timer, OnExitFn}
    end,

    OnNewBlockFn = fun(Block) -> self() ! {new_block, Block} end,

    PopVerifier = new(PopConfigData, PopManagerConfig#pop_manager_config{on_new_block = OnNewBlockFn}, PopVerConfig),

    loop(PopVerifier, Data).

loop(State, Data = {TimerRef, OnExitFn}) ->
    %% ?debugHere,
    %% ?debugFmt("verifier loop ~p~n", [self()]),
    receive 
 	{net, SenderAddress, CurrentTime, MsgId, NetData} ->
	    NewState = on_net_message(SenderAddress, CurrentTime, MsgId, NetData, State);
	{timer_custom, CurrentTime} ->
	    NewState = on_timer(CurrentTime, State);
	timer_real ->
	    CurrentTime = erlang:system_time(second),
	    NewState = on_timer(CurrentTime, State);
	{new_block, Block} ->
	    NewState = on_new_block(Block, State);
	exit ->
	    if TimerRef /= no_timer ->
		    {ok, cancel} = timer:cancel(TimerRef);
	       true -> ok
	    end,
	    NewState = exit;
	Any ->
	    NewState = erlang:error({"unexpected message", Any})
    end,
    
    if NewState /= exit ->
	    loop(NewState, Data);
       true -> 
	    %% ?debugFmt("exit verifier loop ~p~n", [self()]),
	    OnExitFn(),
	    ok
    end.
	    
