-module(pop_protocol_test).
  
-include_lib("eunit/include/eunit.hrl").

-import(pop_protocol, [add_one_block/3, get_genesis_tree_data/1]).
-import(blocktree, [add_new_block/2, get_block_by_id/2]).
-import(my_crypto, [hash/1, sign/2, verify/3, read_file_key/2]).
-import(my_serializer, [serialize_object/1]).

-include_lib("stdlib/include/assert.hrl").

-include("../src/potato_records.hrl").

make_block(PrevId, PrivateKey, PublicKey, Index, Time) ->
    Block0 = #{
       previous_id => PrevId,
       height => 1,
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



pop_protocol_test() ->
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

    Block = make_block(genesis, PrivateKey, PublicKey, 1, 110),

    pop_protocol:add_one_block(Block, 115, PD0),
    ok.
