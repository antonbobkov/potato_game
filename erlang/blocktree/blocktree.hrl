-record(transaction, {game_data, nonce, player_id, consensus_data}).
-record(block, {previous_id, this_id, height, transactions, consensus_data}).

-record(verifier_data, {block_map, transaction_map}).
