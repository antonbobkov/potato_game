-module(pop_verifier_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").


make_verifier_array() ->
    %% make verifier array, they share keys

    PrivateKey = my_crypto:read_file_key(private, "key1.prv"),
    PublicKey = my_crypto:read_file_key(public, "key1.pub"),

    VerFunc = fun(Index) -> 
		      #verifier_public_info{
			 index = Index, 
			 public_key = PublicKey,
			 network_data = {verifier, Index}
			} 
	      end,
    VerifierArr = array:from_list(lists:map(VerFunc, [0, 1, 2, 3, 4])),

    {VerifierArr, PrivateKey, PublicKey}.


start_pv(VerifierArr, PrivateKey, VerifierIndex, MessageHandlerPid, _TrackerFn = {OnStartFn, OnExitFn}) ->

    CurrentTime = 100,

    MyAddress = (array:get(VerifierIndex, VerifierArr))#verifier_public_info.network_data,

    PopChainConfig = #pop_config_data{
		   time_between_blocks = 10, 
		   time_desync_margin = 5, 
		   chain_id = hype_chain, 
		   verifiers_arr = VerifierArr, 
		   init_time = CurrentTime
		  },

    PopManagerConfig = #pop_manager_config{
			  request_range_backup = 3,

			  net_send = fun(DestAddress, Id, Data) -> MessageHandlerPid ! {net, {MyAddress, DestAddress, Id, Data}} end,

			  on_new_block = undefined
			 },

    PopVerifierConfig = #pop_verifier_config{
			   sub_time_out = 20,
			   my_index = VerifierIndex,
			   my_key = PrivateKey
			  },

    VerifierPid = spawn_link(fun() -> 
				     OnStartFn({verifier, VerifierIndex}),

				     pop_verifier:start_loop( 
				       PopChainConfig, 
				       PopManagerConfig, 
				       PopVerifierConfig, 
				       no_timer, 
				       OnExitFn
				      )
			     end),

    VerifierPid.


extract_message(Timeout) ->
    receive
	Msg -> 
	    Msg
    after Timeout -> 
	    none
    end.

%% extract_message() -> extract_message(0).

%% dump_all_messages() ->    
%%     receive
%% 	_Msg -> 
%% 	    dump_all_messages()
%%     after 0 ->
%% 	    done
%%     end.

no_more_messages() ->
    receive
	_Any ->
	    false
    after 0 -> 
	    true
    end.


msg_handler_setup_test() ->
    TrackingData = pid_tracker:start(),

    {_, TrackingFn} = TrackingData,

    MsgPid = local_message_relay_manager:start(TrackingFn),

    MsgPid ! exit,

    ?assertEqual(all_done, pid_tracker:finish(TrackingData)),

    ok.

msg_handler_message_test() ->
    TrackingData = pid_tracker:start(),

    {_, TrackingFn} = TrackingData,
    
    Pid = local_message_relay_manager:start(TrackingFn),

    Pid ! {add_pid, me_fr, self()},
    Pid ! {add_pid, me_to, self()},

    Pid ! {net, {me_fr, me_to, test_msg_1, test_data} },
    Pid ! {net, {me_fr, me_to, test_msg_2, test_data} },

    Pid ! {send_buffered_messages, 100},

    ?assertEqual({net, me_fr, 100, test_msg_1, test_data}, extract_message(100)),
    ?assertEqual({net, me_fr, 100, test_msg_2, test_data}, extract_message(100)),

    ?assert(no_more_messages()),

    Pid ! exit,

    ?assertEqual(exit, extract_message(100)),
    ?assertEqual(exit, extract_message(100)),

    ?assert(no_more_messages()),

    ?assertEqual(all_done, pid_tracker:finish(TrackingData)),

    ok.

verifier_start_up_test() ->
    TrackingData = pid_tracker:start(),

    {_, TrackingFn} = TrackingData,
    
    {VerifierArr, PrivateKey, _PublicKey} = make_verifier_array(),

    VerifierPid = start_pv(VerifierArr, PrivateKey, 0, none, TrackingFn),

    VerifierPid ! exit,

    ?assert(no_more_messages()),

    ?assertEqual(all_done, pid_tracker:finish(TrackingData)),

    ok.
    
start_up_test() ->
    TrackingData = pid_tracker:start(),

    {_, TrackingFn} = TrackingData,

    MessageHandlerPid = local_message_relay_manager:start(TrackingFn),

    {VerifierArr, PrivateKey, _PublicKey} = make_verifier_array(),

    VerifierPid = start_pv(VerifierArr, PrivateKey, 0, MessageHandlerPid, TrackingFn),

    MessageHandlerPid ! {add_pid, {verifier, 0}, VerifierPid},

    MessageHandlerPid ! exit,

    ?assert(no_more_messages()),

    ?assertEqual(all_done, pid_tracker:finish(TrackingData)),

    ok.

initialize_all() ->
    {_, TrackingFn} = TrackingData = pid_tracker:start(),

    MessageHandlerPid = local_message_relay_manager:start(TrackingFn),

    {VerifierArr, PrivateKey, _PublicKey} = make_verifier_array(),

    StartVerifierFn = fun(Index, _) ->
			      VerifierPid = start_pv(VerifierArr, PrivateKey, Index, MessageHandlerPid, TrackingFn),
			      MessageHandlerPid ! {add_pid, {verifier, Index}, VerifierPid},
			      ok
		      end,

    array:map(StartVerifierFn, VerifierArr),

    ?assert(no_more_messages()),

    {TrackingData, MessageHandlerPid}.

finalize_all(_InitData = {TrackingData, MessageHandlerPid}) ->
    ?assert(no_more_messages()),

    MessageHandlerPid ! exit,

    ?assertEqual(all_done, pid_tracker:finish(TrackingData)),

    ?assert(no_more_messages()),

    ok.
    

start_up_all_test() ->
    InitData = initialize_all(),

    finalize_all(InitData),

    ok.

new_block_match_test(Msg, VerNum, Ht) ->
    case Msg of
	{{verifier, VerNum}, {verifier, _}, send_full_blocks, {new, [Block]}} when is_map(Block) ->
	    BHt = maps:get(height, Block),
	    if BHt == Ht ->
		    ok;
	       true ->
		    {bad_height, BHt, Ht}
	    end;
	_Else ->
	    {bad_match, VerNum, Msg}
    end.
	    
    
simulation_test() ->

    {_, MessageHandlerPid} = InitData = initialize_all(),

    MessageHandlerPid ! {timer_tick, 110},

    MessageHandlerPid ! {set_hook, 5, self()},

    {msg_buffer, Msgs} = extract_message(100),

    lists:foreach(fun(M) -> ?assertEqual(ok, new_block_match_test(M, 1, 1)) end, Msgs),

    MessageHandlerPid ! {send_buffered_messages, 110},

    MessageHandlerPid ! {timer_tick, 120},

    MessageHandlerPid ! {set_hook, 5, self()},

    {msg_buffer, Msgs2} = extract_message(100),

    lists:foreach(fun(M) -> ?assertEqual(ok, new_block_match_test(M, 2, 2)) end, Msgs2),

    finalize_all(InitData),

    ok.
