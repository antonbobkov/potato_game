-module(pop_protocol).

%-export([add_new_transaction/2, add_new_block/2, generate_new_block/2]).

-include("../blocktree/blocktree.hrl").
-include_lib("stdlib/include/assert.hrl").

-record(protocol_data, {verifiers_lst, time_between_blocks, chain_id, tree_data}).
-record(consensus_block_data, {signature, signer_pub_key, timestamp}).

add_one_block(ProtocolData, Block)
  when 
      is_record(ProtocolData, protocol_data), 
      is_record(Block, block) 
      ->

    % check if this block is already in the chain
    % if so, ignore

    #protocol_data{
       verifiers_lst = VerifiersLst, 
       time_between_blocks = Time, 
       chain_id = MainChainId, 
       tree_data = TD
      } = ProtocolData,

    % add this block to tree_data

    #block{
       previous_id = PrevId, 
       this_id = ThisId, 
       transactions = BlockTransactionsList,
       consensus_data = #consensus_block_data{
			   signature = Sgn,
			   signer_pub_key = SgnKey,
			   timestamp = Tmp
			  }
      } = Block,
    
% get previous block
% check ThisId hash correctness
% check signature's correctness
% check that signer is one of the verifiers
% check that the time is correct for this verifier
% should be larger, and have the correct remainder
% (OPTIONAL) check sequence of different verifiers

% Look at transactions
% verify signature's correctness
% compare ChainId

    pass.


