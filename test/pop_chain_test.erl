-module(pop_chain_test).
  
-include_lib("eunit/include/eunit.hrl").

%% -export([basic_test/0, generate_block_test/0]).

-include_lib("stdlib/include/assert.hrl").

-include("../src/potato_records.hrl").

make_transaction(PrivateKey, PublicKey, Nonce, ChainId) ->
    T0 = #{
	   game_data => undefined,
	   nonce => Nonce,
	   player_id => PublicKey,
	   consensus_data => #{
			       signature => undefined,
			       chain_id => ChainId
			      }
	  },
    Hash = my_crypto:hash(my_serializer:serialize_object(T0)),
    Signature = my_crypto:sign(Hash, PrivateKey),
    T1 = pop_chain:apply_transaction_signature(Signature, T0),

    T1.

make_block(PrevId, Height, PrivateKey, PublicKey, Index, Time) ->
    make_block(PrevId, Height, PrivateKey, PublicKey, Index, Time, []).

make_block(PrevId, Height, PrivateKey, PublicKey, Index, Time, Transactions) ->
    Block0 = #{
       previous_id => PrevId,
       height => Height,
       this_id => undefined,
       transactions => Transactions,
       consensus_data => #{
			   signature => undefined,
			   verifier_pub_key => PublicKey,
			   verifier_index => Index,
			   timestamp => Time
			  }
     },
    Hash = my_crypto:hash(my_serializer:serialize_object(Block0)),
    Signature = my_crypto:sign(Hash, PrivateKey),

    CD = maps:get(consensus_data, Block0),

    Block1 = Block0#{
		     this_id := Hash,
		     consensus_data := CD#{signature := Signature}
		    },
    Block1.

make_verifier_array() ->
    %% make verifier array, they share keys
    PrivateKey = my_crypto:read_file_key(private, "key1.prv"),
    PublicKey = my_crypto:read_file_key(public, "key1.pub"),
    VerFunc = fun(Index) -> #verifier_public_info{index = Index, public_key = PublicKey} end,
    VerifierArr = array:from_list(lists:map(VerFunc, [0, 1, 2, 3, 4])),

    {VerifierArr, PrivateKey, PublicKey}.
    

generate_block(Index, PrivateKey, PD) ->
    B0 = pop_chain:generate_new_block(Index, PD),
    Sign = my_crypto:sign(maps:get(this_id, B0), PrivateKey),
    B1 = pop_chain:apply_block_signature(Sign, B0),
    B1.
    

basic_test() ->
    {VerifierArr, PrivateKey, PublicKey} = make_verifier_array(),

    CurrentTime = 100,

    PD0 = pop_chain:new(#pop_config_data{
			      time_between_blocks = 10, 
			      time_desync_margin = 5, 
			      chain_id = hype_chain, 
			      verifiers_arr = VerifierArr, 
			      init_time = CurrentTime
			     }),

    ?assertEqual(pop_chain:get_verfier_next_block_time(0, PD0), 150),
    ?assertEqual(pop_chain:get_verfier_next_block_time(1, PD0), 110),

    B1 = make_block(genesis, 1, PrivateKey, PublicKey, 1, 110),
    B2 = make_block(genesis, 1, PrivateKey, PublicKey, 2, 120),
    B3 = make_block(maps:get(this_id, B1), 2, PrivateKey, PublicKey, 2, 120),
    B4 = make_block(maps:get(this_id, B2), 2, PrivateKey, PublicKey, 3, 130),
    B5 = make_block(maps:get(this_id, B4), 3, PrivateKey, PublicKey, 4, 140),

    ?assertEqual(B1, generate_block(1, PrivateKey, PD0)),
    ?assertEqual(B2, generate_block(2, PrivateKey, PD0)),

    FoldFn = fun(Block, PD) -> 
		     CD = maps:get(consensus_data, Block),
		     T = maps:get(timestamp, CD),
		     pop_chain:add_block_in_order(Block, T + 1, PD) 
	     end,

    lists:foldl(FoldFn, PD0, [B1]),
    lists:foldl(FoldFn, PD0, [B1, B3]),
    lists:foldl(FoldFn, PD0, [B1, B2, B3, B4, B5]),

    PD01 = lists:foldl(FoldFn, PD0, [B1]),
    ?assertEqual(B3, generate_block(2, PrivateKey, PD01)),

    PD02 = lists:foldl(FoldFn, PD0, [B2]),
    ?assertEqual(B4, generate_block(3, PrivateKey, PD02)),


    PD1 = lists:foldl(FoldFn, PD0, [B2, B4, B1, B3, B5]),

    ?assert(PD1#pop_chain.head_block == B5),

    ?assertEqual(pop_chain:get_verfier_next_block_time(0, PD1), 150),
    ?assertEqual(pop_chain:get_verfier_next_block_time(2, PD1), 170),
    ?assertEqual(pop_chain:get_verfier_next_block_time(4, PD1), 190),

    TD = PD1#pop_chain.tree_data,

    ?assert(pop_chain:resolve_fork(B4, B5, TD) == B5),
    ?assert(pop_chain:resolve_fork(B1, B2, TD) == B1),
    ?assert(pop_chain:resolve_fork(B3, B4, TD) == B3),


    ?assertError(_, lists:foldl(FoldFn, PD0, [B3])),
    ?assertError(_, lists:foldl(FoldFn, PD0, [B2, B5])),
    ?assertError(_, lists:foldl(FoldFn, PD0, [B1, B1])),

    E1 = make_block(genesis, 1, PrivateKey, PublicKey, 1, 120),
    E2 = make_block(genesis, 2, PrivateKey, PublicKey, 1, 110),
    E3 = make_block(maps:get(this_id, B1), 1, PrivateKey, PublicKey, 2, 120),

    ?assertError(_, lists:foldl(FoldFn, PD0, [E1])),
    ?assertError(_, lists:foldl(FoldFn, PD0, [E2])),
    ?assertError(_, lists:foldl(FoldFn, PD0, [B1, E3])),

    %% same verifier adds two different blocks
    T = make_transaction(PrivateKey, PublicKey, 0, hype_chain),
    B1T = make_block(genesis, 1, PrivateKey, PublicKey, 1, 110, [T]),
    PDT = lists:foldl(FoldFn, PD0, [B1, B1T]),

    MinHash = min(maps:get(this_id, B1), maps:get(this_id, B1T)),
    HeadHash = maps:get(this_id, PDT#pop_chain.head_block),
    ?assertEqual(MinHash, HeadHash),

    ok.

%% get entry in block's consensus data
get_block_cd(Entry, Block) -> maps:get(Entry, maps:get(consensus_data, Block)).


generate_block_test() ->
    %% ?debugHere,
    %% ?debugVal("hi"),
    {VerifierArr, PrivateKey, _} = make_verifier_array(),

    CurrentTime = 100,

    PD0 = pop_chain:new(#pop_config_data{
			      time_between_blocks = 10, 
			      time_desync_margin = 5, 
			      chain_id = hype_chain, 
			      verifiers_arr = VerifierArr, 
			      init_time = CurrentTime
			     }),

    MakeChainFn = fun(VerifierIndex, PD) -> 
			  Block = generate_block(VerifierIndex, PrivateKey, PD),
			  %% io:format(user, "B ~p ~n", [Block]),
			  %% io:format(user, "~n~n", []),
			  pop_chain:add_block_in_order(Block, get_block_cd(timestamp, Block) + 1, PD) 
		  end,

    lists:foldl(MakeChainFn, PD0, [1]),
    lists:foldl(MakeChainFn, PD0, [1, 0]),
    lists:foldl(MakeChainFn, PD0, [1, 1]),
    lists:foldl(MakeChainFn, PD0, [1, 0, 2, 4, 0, 3, 1]),
    
    ok.
