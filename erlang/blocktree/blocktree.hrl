%% -record(transaction, {game_data, nonce, player_id, consensus_data}).
%% -record(block, {previous_id, this_id, height, transactions, consensus_data}).

-record(tree_data, {transaction_map=maps:new(), block_map=maps:new()}).
