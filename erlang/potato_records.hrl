%% -record(transaction, {game_data, nonce, player_id, consensus_data}).
%% -record(block, {previous_id, this_id, height, transactions, consensus_data}).
-record(tree_data, {transaction_map=maps:new(), block_map=maps:new()}).

-record(verifier_public_info, {index, public_key, network_data}).
-record(protocol_data, {verifiers_arr, time_between_blocks, time_desync_margin, chain_id, tree_data}).
%% -record(consensus_block_data, {signature, verifier_pub_key, verifier_index, timestamp}).
