-module(pop_protocol).

-export([add_new_block/3, get_genesis_tree_data/1]).

-include_lib("stdlib/include/assert.hrl").

-include("../potato_records.hrl").

map_key_match_assert(Map, KeyList) ->
    K1 = lists:sort(maps:keys(Map)),
    K2 = lists:sort(KeyList),
    ?assertEqual(K1, K2, {"key mismatch", K1, K2}),
    ok.

transaction_map_structure_assert(T) when is_map(T) ->
    map_key_match_assert(T, [game_data, nonce, player_id, consensus_data]),
    
    CD = maps:get(consensus_data, T),
    map_key_match_assert(CD, [signature, chain_id]),
    
    ok.

check_block_map_structure(B) when is_map(B) ->
    map_key_match_assert(B, [previous_id, this_id, height, transactions, consensus_data]),
    
    CD = maps:get(consensus_data, B),
    map_key_match_assert(CD, [signature, verifier_pub_key, verifier_index, timestamp]),
    
    TL = maps:get(transactions, B),
    lists:map(fun transaction_map_structure_assert/1, TL),
    
    ok.
    

compute_block_hash(Block) when is_map(Block) ->
    #{consensus_data := CD} = Block,
    CleanBlock = Block#{
	  this_id := undefined, 
	  consensus_data := CD#{signature := undefined}
	 },
    my_crypto:hash( my_serializer:serialize_object(CleanBlock) ).

compute_transaction_hash(Transaction) when is_map(Transaction) ->
    CD = maps:get(consensus_data, Transaction),
    CleanTransaction = Transaction#{
	  consensus_data := CD#{signature := undefined}
	 },
    my_crypto:hash( my_serializer:serialize_object(CleanTransaction) ).

check_transaction_correctness(Transaction, ChainId) when is_map(Transaction) ->
    #{
      consensus_data := CD,
      player_id := PlrKey
     } = Transaction,

    #{
      signature := PlrSgn,
      chain_id := TrChainId
     } = CD,

    Hash = compute_transaction_hash(Transaction),

    %% verify signature's correctness
    ?assert(my_crypto:verify(Hash, PlrSgn, PlrKey), "transaction signature failed verification"),

    %% compare ChainId
    ?assertEqual(TrChainId, ChainId, "bad transaction ChainId"),

    ok.
    

add_new_block(Block, CurrentTime, ProtocolData)
  when 
      is_record(ProtocolData, protocol_data), 
      is_map(Block)
      ->

    check_block_map_structure(Block),

    #protocol_data{
       verifiers_arr = VerifiersArr, 
       time_between_blocks = TimeBetween, 
       time_desync_margin = TimeDesyncMargin,
       chain_id = MainChainId, 
       tree_data = TD0
      } = ProtocolData,

    

    #{
       previous_id := PrevId, 
       this_id := ThisId, 
       transactions := BlockTransactionsList,
       consensus_data := #{
			   signature := VerSgn,
			   verifier_pub_key := VerKey,
			   verifier_index := VerIndex,
			   timestamp := Tmp
			  }
      } = Block,
    
    
    %% check that signer is one of the verifiers
    #verifier_public_info{
       index = VerIndexChk, 
       public_key = PubKeyChk
      } = array:get(VerIndex, VerifiersArr),

    ?assertEqual(VerIndex, VerIndexChk, "indices don't match"),
    ?assertEqual(VerKey, PubKeyChk, "keys don't match"),


    %% check that the time is correct for this verifier
    %% (also check that it is not too far into the future)
    %% should be larger, and have the correct remainder
    
    PreviousBlock = blocktree:get_block_by_id(PrevId, TD0),
    PreviousBlockTimestamp = maps:get(timestamp, maps:get(consensus_data, PreviousBlock)),

    ?assert(PreviousBlockTimestamp < Tmp, "time should be larger than previous"),
    ?assert(Tmp - TimeDesyncMargin < CurrentTime, "block cannot be in the future"),

    %% Check that this verifier hasn't already submitted a block here
    ChildList = blocktree:get_children_block_list(PrevId, TD0),
    IndexFn = fun(B) -> maps:get(verifier_index, maps:get(consensus_data, B)) end,
    VerIndexList = lists:map(IndexFn, ChildList),
    ?assertEqual(lists:member(VerIndex, VerIndexList), false),
    
    VerNum = array:size(VerifiersArr),
    ?assertEqual(Tmp rem (TimeBetween * VerNum), TimeBetween * VerIndex, "bad time for that verifier"),
    
    %% check ThisId hash correctness
    Hash = ThisId,
    ?assertEqual(Hash, compute_block_hash(Block), "incorrect hash"),

    %% check signature's correctness
    ?assert(my_crypto:verify(Hash, VerSgn, VerKey), "signature failed verification"),

    %% (OPTIONAL) check sequence of different verifiers

    %% Verify transactions
    lists:map(fun(T) -> check_transaction_correctness(T, MainChainId) end, BlockTransactionsList),

    %% add this block to tree_data 
    %% this can trigger errors if the block if poorly formed
    %% also fails if block is orphan or already exists
    TD1 = blocktree:add_new_block(Block, TD0),

    NewProtocolData = ProtocolData#protocol_data{tree_data = TD1},

    NewProtocolData.

get_genesis_tree_data(CurrentTime) ->
    TD0 = #tree_data{},
    B0 = blocktree:generate_new_block(undefined, TD0),
    B1 = B0#{
	     this_id := genesis,
	     consensus_data := #{
				 timestamp => CurrentTime,
				 signature => undefined, 
				 verifier_pub_key => undefined, 
				 verifier_index => undefined
				}
	    },
    check_block_map_structure(B1),
    TD1 = blocktree:add_new_block(B1, TD0),
    TD1.

    
