-module(pop_protocol).

-export([add_one_block/3]).
%%-import(blocktree, [add_new_transaction/2, add_new_block/2, generate_new_block/2]).

-import(blocktree, [add_new_block/2, get_block_by_id/2]).
-import(my_crypto, [hash/1, sign/2, verify/3]).
-import(my_serializer, [serialize_object/1]).


-include("../blocktree/blocktree.hrl").
-include_lib("stdlib/include/assert.hrl").

-record(verifier_public_info, {index, public_key, network_data}).
-record(protocol_data, {verifiers_arr, time_between_blocks, time_desync_margin, chain_id, tree_data}).
%% -record(consensus_block_data, {signature, verifier_pub_key, verifier_index, timestamp}).

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
    

add_one_block(ProtocolData, Block, CurrentTime)
  when 
      is_record(ProtocolData, protocol_data), 
      is_map(Block)
      ->


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
    
    PreviousBlock = blocktree:get_block_by_id(TD0, PrevId),
    PreviousBlockTimestamp = maps:get(timestamp, maps:get(consensus_data, PreviousBlock)),

    ?assert(PreviousBlockTimestamp < Tmp, "time should be larger than previous"),
    ?assert(Tmp - TimeDesyncMargin < CurrentTime, "block cannot be in the future"),
    
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

    TD1.


