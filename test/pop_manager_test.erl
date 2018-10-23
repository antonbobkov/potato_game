-module(pop_manager_test).
  
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../src/potato_records.hrl").

make_verifier_array() ->
    %% make verifier array, they share keys
    PrivateKey = my_crypto:read_file_key(private, "key1.prv"),
    PublicKey = my_crypto:read_file_key(public, "key1.pub"),
    VerFunc = fun(Index) -> #verifier_public_info{index = Index, public_key = PublicKey} end,
    VerifierArr = array:from_list(lists:map(VerFunc, [0, 1, 2, 3, 4])),

    {VerifierArr, PrivateKey, PublicKey}.

%% get entry in block's consensus data
get_block_cd(Entry, Block) -> maps:get(Entry, maps:get(consensus_data, Block)).


make_pm() ->
    {VerifierArr, PrivateKey, PublicKey} = make_verifier_array(),

    CurrentTime = 100,

    PopChainConfig = #pop_config_data{
		   time_between_blocks = 10, 
		   time_desync_margin = 5, 
		   chain_id = hype_chain, 
		   verifiers_arr = VerifierArr, 
		   init_time = CurrentTime
		  },

    PopManagerConfig = #pop_manager_config{
			  request_range_backup = 3,

			  %% send message to PiD in Address via !
			  net_send = fun(Address, Id, Data) -> Address ! {net, Id, Data} end,


			  %% when new block is added send its hash message to itself 
			  on_new_block = fun(Block) -> self() ! {block, maps:get(this_id, Block)} end
			 },

    PM = pop_manager:new(PopChainConfig, PopManagerConfig),

    {PM, PrivateKey, PublicKey}.

extract_message() ->
    receive
	Msg -> 
	    Msg
    after 0 -> 
	    none
    end.

dump_all_messages() ->    
    receive
	_Msg -> 
	    dump_all_messages()
    after 0 ->
	    done
    end.


basic_test() ->
    {PM, PrivateKey, _PublicKey} = make_pm(),
    
    PC = PM#pop_manager.pop_chain,

    B1 = pop_chain:generate_new_block(1, PC),

    B2 = pop_chain:apply_block_signature(my_crypto:sign(maps:get(this_id, B1), PrivateKey), B1),

    T1 = pop_chain:get_verfier_next_block_time(1, PC),

    pop_manager:on_net_message(self(), T1, send_full_blocks, {new, [B2]}, PM),

    dump_all_messages(),
	 
    ok.

%% generate a sequence of blocks starting at the end of PC0
%% NewBlockCount many new blocks are generated
%% first block is generated by verifier InitialVerifierNum
%% and then we rotate through verifiers in order

generate_chain(PC0, NewBlockCount, InitialVerifierNum, PrivateKey) ->

    MapAccFn = fun(PC, VerNum) ->
		       Block = pop_chain:generate_new_block(VerNum, PC),
		       AddTime = get_block_cd(timestamp, Block),

		       SignedBlock = pop_chain:apply_block_signature(my_crypto:sign(maps:get(this_id, Block), PrivateKey), Block),

		       {pop_chain:add_block_in_order(SignedBlock, AddTime, PC), SignedBlock}
	       end,

    TotalVerifiersCount = array:size(PC0#pop_chain.pop_config_data#pop_config_data.verifiers_arr),

    NumSeq = lists:seq(InitialVerifierNum, InitialVerifierNum + NewBlockCount - 1),
    VerSeq = lists:map(fun(N) -> N rem TotalVerifiersCount end, NumSeq),

    {PC1, BlockList} = potato_haskell:mapAccumL(MapAccFn, PC0, VerSeq), 

    {PC1, BlockList}.

get_hashes(BlockList) ->
    lists:map(fun(B) -> maps:get(this_id, B) end, BlockList).

get_hashes_tuple(BlockList) -> list_to_tuple(get_hashes(BlockList)).

%% extract block messages, put them in a list
extract_new_block_messages() ->
    receive
	{block, Hash} -> 
	    [Hash | extract_new_block_messages()];
	_Any ->
	    fail
    after 0 -> 
	    []
    end.

no_more_messages() ->
    receive
	_Any ->
	    false
    after 0 -> 
	    true
    end.
    

fancy_test() ->
    {PM, PrivateKey, _PublicKey} = make_pm(),
    PC = PM#pop_manager.pop_chain,
    Time = 2000,

    A0 = pop_chain:get_head_block(PC),

    generate_chain(PC, 1, 0, PrivateKey),

    {PC_A, [A1, A2]} = generate_chain(PC, 2, 0, PrivateKey),

    {PC_B, [B1, B2, B3, B4]} = generate_chain(PC_A, 4, 0, PrivateKey),

    {PC_C, [C1, C2, C3, C4, C5]} = generate_chain(PC_A, 5, 1, PrivateKey),

    PM_B = PM#pop_manager{pop_chain = PC_B},

    ?assertEqual(B4, pop_chain:get_head_block(PM_B#pop_manager.pop_chain)),



    %% Test on_net_message send_block_hashes

    PM_Bm = pop_manager:on_net_message(self(), Time, send_block_hashes, get_hashes([A2, C3, B4, C1]), PM_B),

    ?assertEqual({net, request_full_blocks, get_hashes([C3, C1])}, extract_message()),

    ?assertEqual(PM_Bm, PM_B),

    ?assert(no_more_messages()),


    %% Test on_net_message send_full_blocks in order

    PM_BC = pop_manager:on_net_message(self(), Time, send_full_blocks, {old, [C1, C2, C3, C4, C5]}, PM_B),

    ?assertEqual(get_hashes([C1, C2, C3, C4, C5]), extract_new_block_messages()),

    ?assert(no_more_messages()),


    %% Test on_net_message send_full_blocks out of order with repeats

    PM_BC0 = pop_manager:on_net_message(self(), Time, send_full_blocks, {old, [C2, C3]}, PM_B),

    ?assertEqual(maps:size(PM_BC0#pop_manager.unbound_blocks), 2),

    PM_BCm = pop_manager:on_net_message(self(), Time, send_full_blocks, {old, [C1, C5, C2, C4]}, PM_BC0),

    ?assertEqual(get_hashes([C1, C2, C3, C4, C5]), extract_new_block_messages()),

    ?assertEqual(PM_BCm, PM_BC),

    ?assertEqual(maps:size(PM_BC#pop_manager.unbound_blocks), 0),

    ?assertEqual(C5, pop_chain:get_head_block(PM_BC#pop_manager.pop_chain)),

    ?assert(no_more_messages()),


    %% Test on_net_message send_full_blocks new unknown block
    PM_B_C5 = pop_manager:on_net_message(self(), Time, send_full_blocks, {new, [C5]}, PM_B),

    ?assertEqual(maps:size(PM_B_C5#pop_manager.unbound_blocks), 1),

    ?assertEqual({net, request_block_hash_range, get_hashes_tuple([B4, B1, C5])}, extract_message()),

    ?assert(no_more_messages()),

    ok.

