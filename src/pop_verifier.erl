-module(pop_verifier).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

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
    

init(InitData = {PopConfigData, PopManagerConfig, PopVerConfig}) ->
    TimerIntervalSec = PopVerConfig#pop_verifier_config.timer_interval,

    if TimerIntervalSec == none ->
	    CurrentTime = PopConfigData#pop_config_data.init_time,
	    TimerRef = undefined;
       true ->
	    CurrentTime = undefined,
	    {ok, TimerRef} = timer:send_interval(TimerIntervalSec * 1000, real_timer_tick)
    end,

    OnNewBlockFn = fun(Block) -> self() ! {new_block, Block} end,

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

handle_info({net_udp, Data = {SenderAddress, subscribe, _} }, State) ->

    make_event(net_udp, Data, State),
 
    CurrentTime = get_current_time(State),

    Subs = State#pop_verifier.subscribers,

    NewState = State#pop_verifier{subscribers = maps:put(SenderAddress, CurrentTime, Subs)},

    {noreply, NewState};

handle_info({net_udp, Data = {SenderAddress, MsgId, NetData} }, State) ->

    make_event(net_udp, Data, State),

    CurrentTime = get_current_time(State),

    PM0 = State#pop_verifier.pop_manager,

    PM1 = pop_manager:on_net_message(SenderAddress, CurrentTime, MsgId, NetData, PM0),
    
    NewState = State#pop_verifier{pop_manager = PM1},

    {noreply, NewState};

handle_info({new_block, NewBlock}, State) ->
    make_event(new_block, NewBlock, State),

    Sz = maps:size(State#pop_verifier.subscribers),

    if Sz /= 0 ->
	    emit_net_message(subs, send_full_blocks, {new, NewBlock}, State);

       true -> 
	    ok
    end,
				 
    {noreply, State};

handle_info(real_timer_tick, State) ->
    ?assertNotEqual(undefined, State#pop_verifier.timer_ref),

    CurrentTime = erlang:system_time(second),

    make_event(real_timer_tick, CurrentTime, State),

    NewState = on_timer(CurrentTime, State),

    {noreply, NewState};

handle_info({custom_timer_tick, CurrentTime}, State) ->
    ?assertEqual(undefined, State#pop_verifier.timer_ref),

    make_event(custom_timer_tick, CurrentTime, State),

    State1 = on_timer(CurrentTime, State),

    State2 = State1#pop_verifier{current_time = CurrentTime},

    {noreply, State2};

handle_info(Data, State) ->
    erlang:error(unexpected_handle_info, [Data, State]).

handle_cast(Data, State) ->
    erlang:error(unexpected_handle_cast, [Data, State]).

handle_call(E, From, S) ->
    erlang:error(unexpected_handle_call, [E, From, S]).

code_change(OldVsn, State, Extra) ->
    erlang:error(unexpected_code_change, [OldVsn, State, Extra]).

terminate(Reason, State) ->

    TimerRef = State#pop_verifier.timer_ref,

    if TimerRef /= undefined ->
	    {ok, cancel} = timer:cancel(TimerRef);
       true -> ok
    end,

    make_event(terminate, Reason, State),

    ok.
