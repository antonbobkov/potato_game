-module(pop_protocol).

%-export([add_new_transaction/2, add_new_block/2, generate_new_block/2]).
%-import(blocktree, [add_new_transaction/2, add_new_block/2, generate_new_block/2]).

-import(blocktree, [add_new_block/2, get_block_by_id/2]).
-import(my_crypto, [hash/1, sign/2, verify/3]).


-include("../blocktree/blocktree.hrl").
-include_lib("stdlib/include/assert.hrl").

-record(verifier_public_info, {index, public_key, network_data}).
-record(protocol_data, {verifiers_arr, time_between_blocks, time_desync_margin, chain_id, tree_data}).
-record(consensus_block_data, {signature, verifier_pub_key, verifier_index, timestamp}).

compute_block_hash(Block) 
  when 
      is_record(Block, block) 
      ->
    B = Block#block{
	  this_id=undefined, 
	  consensus_data = #consensus_block_data{signature=undefined}
	 },
    my_crypto:hash(B).

add_one_block(ProtocolData, Block, CurrentTime)
  when 
      is_record(ProtocolData, protocol_data), 
      is_record(Block, block) 
      ->


    #protocol_data{
       verifiers_arr = VerifiersArr, 
       time_between_blocks = TimeBetween, 
       time_desync_margin = TimeDesyncMargin,
       chain_id = MainChainId, 
       tree_data = TD0
      } = ProtocolData,

    % add this block to tree_data 
    % this can trigger errors if the block if poorly formed
    % also fails if block is orphan or already exists
    TD1 = blocktree:add_new_block(Block, TD0),
    

    #block{
       previous_id = PrevId, 
       this_id = ThisId, 
       transactions = BlockTransactionsList,
       consensus_data = #consensus_block_data{
			   signature = VerSgn,
			   verifier_pub_key = VerKey,
			   verifier_index = VerIndex,
			   timestamp = Tmp
			  }
      } = Block,
    
    
    % check that signer is one of the verifiers
    #verifier_public_info{
       index = VerIndexChk, 
       public_key = PubKeyChk
      } = array:get(VerIndex, VerifiersArr),

    ?assertEqual(VerIndex, VerIndexChk, "indices don't match"),
    ?assertEqual(VerKey, PubKeyChk, "keys don't match"),


    % check that the time is correct for this verifier
    % (also check that it is not too far into the future)
    % should be larger, and have the correct remainder
    
    PreviousBlock = blocktree:get_block_by_id(TD0, PrevId),
    PreviousBlockTimestamp = PreviousBlock#block.consensus_data#consensus_block_data.timestamp,

    ?assert(PreviousBlockTimestamp < Tmp, "time should be larger than previous"),
    ?assert(Tmp - TimeDesyncMargin < CurrentTime, "block cannot be in the future"),
    
    VerNum = array:size(VerifiersArr),
    ?assertEqual(Tmp rem (TimeBetween * VerNum), TimeBetween * VerIndex, "bad time for that verifier"),
    
    

    % check ThisId hash correctness
    ?assertEqual(ThisId, compute_block_hash(Block), "incorrect hash"),

% check signature's correctness

% (OPTIONAL) check sequence of different verifiers

% Look at transactions
% verify signature's correctness
% compare ChainId

    pass.


