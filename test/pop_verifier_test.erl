-module(pop_verifier_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").


make_verifier_array(VerNum) ->
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
    VerifierArr = array:from_list(lists:map(VerFunc, lists:seq(0, VerNum - 1))),

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

			  %% fix,
			  net_multi_send = fun(DestAddress, Id, Data) -> MessageHandlerPid ! {net, {MyAddress, DestAddress, Id, Data}} end,

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

    Pid ! {process_buffered_messages_no_reply, 100},

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
    
    {VerifierArr, PrivateKey, _PublicKey} = make_verifier_array(5),

    VerifierPid = start_pv(VerifierArr, PrivateKey, 0, none, TrackingFn),

    VerifierPid ! exit,

    ?assert(no_more_messages()),

    ?assertEqual(all_done, pid_tracker:finish(TrackingData)),

    ok.
    
start_up_test() ->
    TrackingData = pid_tracker:start(),

    {_, TrackingFn} = TrackingData,

    MessageHandlerPid = local_message_relay_manager:start(TrackingFn),

    {VerifierArr, PrivateKey, _PublicKey} = make_verifier_array(5),

    VerifierPid = start_pv(VerifierArr, PrivateKey, 0, MessageHandlerPid, TrackingFn),

    MessageHandlerPid ! {add_pid, {verifier, 0}, VerifierPid},

    MessageHandlerPid ! exit,

    ?assert(no_more_messages()),

    ?assertEqual(all_done, pid_tracker:finish(TrackingData)),

    ok.

initialize_all(VerNum) ->
    {_, TrackingFn} = TrackingData = pid_tracker:start(),

    MessageHandlerPid = local_message_relay_manager:start(TrackingFn),

    {VerifierArr, PrivateKey, _PublicKey} = make_verifier_array(VerNum),

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
    InitData = initialize_all(5),

    finalize_all(InitData),

    ok.

new_block_match_fn(VerNum, Ht) ->
    fun(Msg) ->
	    case Msg of
		{{verifier, VerNum}, {verifier, _}, send_full_blocks, {new, [Block]}} when is_map(Block) ->
		    BHt = maps:get(height, Block),
		    if BHt == Ht ->
			    true;
		       true ->
			    false  %{bad_height, BHt, Ht}
		    end;
		_Else ->
		    false  %{bad_match, VerNum, Msg}
	    end
    end.

%% message_match(MsgList, {total, Count}) ->	    
%%     ?assertEqual(length(MsgList), Count);

message_match(MsgList, {MatchFn, Count}) ->	    
    %% try length(lists:filter(MatchFn, MsgList)) of
    %% 	_ -> ok
    %% catch
    %% 	error:_ ->
    %% 	    lists:foreach(fun(M) -> 
    %% 				  ?debugVal(M),
    %% 				  ?debugVal(MatchFn(M))
    %% 			  end, MsgList),
    %% 	    erlang:exit()
    %% end;
    
    ?assertEqual(length(lists:filter(MatchFn, MsgList)), Count);

message_match(_MsgList, []) ->
    ok;
message_match(MsgList, [H|T]) ->
    message_match(MsgList, H),
    message_match(MsgList, T).

get_total_count([]) ->
    0;
get_total_count({_, Count}) ->
    Count;
get_total_count([H|T]) ->
    get_total_count(H) + get_total_count(T).


check_message_list(MessageHandlerPid, MatchList) ->
    Count = get_total_count(MatchList), 

    MessageHandlerPid ! {set_hook, Count, self()},

    {msg_buffer, MsgList} = extract_message(100),

    ?assertEqual(length(MsgList), Count),

    %% message_match(MsgList, MatchList),

    try message_match(MsgList, MatchList) of
    	_ -> ok
    catch
    	error:Reason ->
	    lists:foreach(fun output_one_message/1, MsgList),
    	    erlang:error(Reason)
    end.
    
basic_simulation_test() ->

    {_, MessageHandlerPid} = InitData = initialize_all(5),

    MessageHandlerPid ! {timer_tick, 110},

    check_message_list(MessageHandlerPid, {new_block_match_fn(1, 1), 5}),

    MessageHandlerPid ! {timer_tick, 120},

    check_message_list(MessageHandlerPid, [{new_block_match_fn(2, 2), 5}]),


    MessageHandlerPid ! {timer_tick, 140},

    check_message_list(MessageHandlerPid, [
					       {new_block_match_fn(3, 3), 5},
					       {new_block_match_fn(4, 3), 5}
					      ]),


    MessageHandlerPid ! {timer_tick, 150},

    check_message_list(MessageHandlerPid, [
					       {new_block_match_fn(4, 4), 5},
					       {new_block_match_fn(0, 4), 5}
					      ]),

    MessageHandlerPid ! {timer_tick, 151},

    check_message_list(MessageHandlerPid, [
					       {new_block_match_fn(0, 5), 5}
					      ]),


    finalize_all(InitData),

    ok.

new_block_basic_match_fn(Msg) ->    
    case Msg of
	{{verifier, _}, {verifier, _}, send_full_blocks, {new, [Block]}} when is_map(Block) ->
            true;
	_Else ->
	    false
    end.

net_message_match_fn(NetMsgId) ->    
    fun(Msg) ->
	    case Msg of
		{{verifier, _}, {verifier, _}, NetMsgId, _} ->
		    true;
		_Else ->
		    false
	    end
    end.
time_tick({MessageHandlerPid, VerNum}, Time) ->

    MessageHandlerPid ! {timer_tick, Time},

    check_message_list(MessageHandlerPid, {fun new_block_basic_match_fn/1, VerNum}).

time_tick_seq(_TTData, TimeStart, TimeEnd, _TimeInc) when TimeStart > TimeEnd ->
    ok;
time_tick_seq(TTData, TimeStart, TimeEnd, TimeInc) ->
    time_tick(TTData, TimeStart),
    time_tick_seq(TTData, TimeStart + TimeInc, TimeEnd, TimeInc).

output_one_message(Msg) ->
    case Msg of
	{{verifier, From}, {verifier, To}, Code, _} ->
	    ?debugFmt("~w -> ~w : ~w~n", [From, To, Code]);
	Else ->
	    erlang:exit(Else)
    end.
													 

output_messages(MessageHandlerPid) ->
    timer:sleep(100),

    MessageHandlerPid ! {process_buffered_messages, self()},

    receive 
	{msg_buffer, MsgList} ->
	    lists:foreach(fun output_one_message/1, MsgList);

	Else ->
	    erlang:exit(Else)
    after 100 ->
	    erlang:exit(timeout)
end.

disconnect_simulation_test() ->
    {_, MessageHandlerPid} = InitData = initialize_all(2),

    MessageHandlerPid ! {add_disconnect, {verifier, 0}, {verifier, 1}},
    MessageHandlerPid ! {add_disconnect, {verifier, 1}, {verifier, 0}},

    TTData = {MessageHandlerPid, 2},

    time_tick_seq(TTData, 110, 160, 10),

    MessageHandlerPid ! {remove_disconnects},

    MessageHandlerPid ! {timer_tick, 180},

    check_message_list(MessageHandlerPid, {fun new_block_basic_match_fn/1, 4}),


    check_message_list(MessageHandlerPid, {net_message_match_fn(request_block_hash_range), 2}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(send_block_hashes), 2}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(request_full_blocks), 2}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(send_full_blocks), 6}),

    time_tick(TTData, 181),
    time_tick(TTData, 190),

    %% output_messages(MessageHandlerPid),

    finalize_all(InitData),

    ok.

disconnect_simulation_2_test() ->
    {_, MessageHandlerPid} = InitData = initialize_all(3),

    MessageHandlerPid ! {add_disconnect, {verifier, 0}, {verifier, 1}},
    MessageHandlerPid ! {add_disconnect, {verifier, 1}, {verifier, 0}},
    MessageHandlerPid ! {add_disconnect, {verifier, 1}, {verifier, 2}},
    MessageHandlerPid ! {add_disconnect, {verifier, 2}, {verifier, 1}},

    TTData = {MessageHandlerPid, 3},

    time_tick_seq(TTData, 110, 150, 10),

    MessageHandlerPid ! {remove_disconnects},

    MessageHandlerPid ! {timer_tick, 160},

    check_message_list(MessageHandlerPid, {fun new_block_basic_match_fn/1, 3}),

    %% output_messages(MessageHandlerPid),

    check_message_list(MessageHandlerPid, {net_message_match_fn(request_block_hash_range), 2}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(send_block_hashes), 2}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(request_full_blocks), 2}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(send_full_blocks), 2}),

    MessageHandlerPid ! {timer_tick, 170},

    check_message_list(MessageHandlerPid, {fun new_block_basic_match_fn/1, 3}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(request_block_hash_range), 1}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(send_block_hashes), 1}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(request_full_blocks), 1}),

    check_message_list(MessageHandlerPid, {net_message_match_fn(send_full_blocks), 4}),

    %% output_messages(MessageHandlerPid),

    finalize_all(InitData),

    ok.
