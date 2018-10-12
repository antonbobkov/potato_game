-record(tree_data, {pending_transactions, block_map}).

-record(verifier_public_info, {index, public_key, network_data}).
-record(protocol_data, {verifiers_arr, time_between_blocks, time_desync_margin, chain_id, tree_data, head_block, orphan_blocks}).

-record(pending_tx, {player_map, counter}).


%% These are handled by maps rather than records
-define(block, [previous_id, this_id, height, transactions, consensus_data]).
-define(consensus_block_data, [signature, verifier_pub_key, verifier_index, timestamp]).

-define(transaction, [game_data, nonce, player_id, consensus_data]).
-define(consensus_transaction_data, [signature, chain_id]).
