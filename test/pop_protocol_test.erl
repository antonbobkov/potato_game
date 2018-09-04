-module(pop_protocol_test).
  
-include_lib("eunit/include/eunit.hrl").

-include_lib("stdlib/include/assert.hrl").

-include("../src/potato_records.hrl").

make_block(PrevId, Height, PrivateKey, PublicKey, Index, Time) ->
    Block0 = #{
       previous_id => PrevId,
       height => Height,
       this_id => undefined,
       transactions => [],
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



add_new_block_test() ->
    %% make verifier array, they share keys
    PrivateKey = my_crypto:read_file_key(private, "key1.prv"),
    PublicKey = my_crypto:read_file_key(public, "key1.pub"),
    VerFunc = fun(Index) -> #verifier_public_info{index = Index, public_key = PublicKey} end,
    VerifierArr = array:from_list(lists:map(VerFunc, [0, 1, 2, 3, 4])),

    CurrentTime = 100,

    PD0 = #protocol_data{
	    verifiers_arr = VerifierArr,
	    time_between_blocks = 10,
	    time_desync_margin = 5,
	    chain_id = hype_chain,
	    tree_data = pop_protocol:get_genesis_tree_data(CurrentTime)
	   },

    B1 = make_block(genesis, 1, PrivateKey, PublicKey, 1, 110),
    B2 = make_block(genesis, 1, PrivateKey, PublicKey, 2, 120),
    B3 = make_block(maps:get(this_id, B1), 2, PrivateKey, PublicKey, 2, 120),
    B4 = make_block(maps:get(this_id, B2), 2, PrivateKey, PublicKey, 3, 130),
    B5 = make_block(maps:get(this_id, B4), 3, PrivateKey, PublicKey, 4, 140),

    FoldFn = fun(Block, PD) -> 
		     CD = maps:get(consensus_data, Block),
		     T = maps:get(timestamp, CD),
		     pop_protocol:add_new_block(Block, T + 1, PD) 
	     end,

    lists:foldl(FoldFn, PD0, [B1]),
    lists:foldl(FoldFn, PD0, [B1, B3]),
    lists:foldl(FoldFn, PD0, [B1, B2, B3, B4, B5]),
    lists:foldl(FoldFn, PD0, [B2, B4, B1, B3, B5]),

    ?assertError(_, lists:foldl(FoldFn, PD0, [B3])),
    ?assertError(_, lists:foldl(FoldFn, PD0, [B2, B5])),
    ?assertError(_, lists:foldl(FoldFn, PD0, [B1, B1])),

    E1 = make_block(genesis, 1, PrivateKey, PublicKey, 1, 120),
    E2 = make_block(genesis, 2, PrivateKey, PublicKey, 1, 110),
    E3 = make_block(maps:get(this_id, B1), 1, PrivateKey, PublicKey, 2, 120),

    ?assertError(_, lists:foldl(FoldFn, PD0, [E1])),
    ?assertError(_, lists:foldl(FoldFn, PD0, [E2])),
    ?assertError(_, lists:foldl(FoldFn, PD0, [B1, E3])),

    ok.
