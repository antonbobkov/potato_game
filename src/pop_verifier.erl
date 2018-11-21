-module(pop_verifier).

-export([
	 new/3,
	 on_net_message/5,
	 on_timer/2,
	 on_new_block/2
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

new(PopConfigData, PopManagerConfig, PopVerConfig) ->
    #pop_verifier{
       subscribers = maps:new(),
       pop_manager = pop_manager:new(PopConfigData, PopManagerConfig),
       config = PopVerConfig,
       verifiers_arr = PopConfigData#pop_config_data.verifiers_arr
      }.

on_net_message(SenderAddress, CurrentTime, subscribe, no_data, PopVerifier) ->
    Subs = PopVerifier#pop_verifier.subscribers,

    PopVerifier#pop_verifier{subscribers = maps:put(SenderAddress, CurrentTime, Subs)}.

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
    emit_net_message_to_list(PopVerifier#pop_verifier.subscribers, MsdId, Data, PopVerifier).

emit_net_message_to_list(AddressList, MsdId, Data, PopVerifier) ->
    NetSendFn = PopVerifier#pop_verifier.pop_manager#pop_manager.config#pop_manager_config.net_send,

    lists:foreach(fun(Address) -> NetSendFn(Address, MsdId, Data) end, AddressList),
    
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

	    emit_net_message(verifiers, send_full_blocks, {new, NewBlock}, PV1),

	    ok;

       true -> ok
    end,
    
    PV1.

on_new_block(NewBlock, PopVerifier) ->
    emit_net_message(subs, send_full_blocks, {new, NewBlock}, PopVerifier),
    PopVerifier.
   
    
		       
		        


