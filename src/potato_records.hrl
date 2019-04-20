-record(tree_data, {pending_transactions, block_map}).
-record(pending_tx, {player_map, counter}).

-record(verifier_public_info,
  {index :: integer(),
  public_key :: public_key:public_key(),
  network_data :: term()}).

-record(pop_config_data, {time_between_blocks, time_desync_margin, chain_id, verifiers_arr, init_time}).

-record(pop_chain, {pop_config_data, tree_data, head_block, genesis_block}).

-record(pop_manager_config, {request_range_backup, 
			     net_multi_send, 
			     on_new_block}).

-record(pop_manager, {unbound_blocks, pop_chain, config}).

-record(pop_verifier_config,
	{
	  sub_time_out, 
	  my_index, 
	  my_key,
	  event_fn,
	  timer_interval
	}).

-record(pop_verifier, 
	{
	  config,
	  current_time,
	  timer_ref,
	  verifiers_arr,
	  subscribers,
	  pop_manager
	}).

-record(game_state,
  {gameid :: integer(),
  validators :: [#verifier_public_info{}],
  otherstuff :: term()}).

-record(potato_monitor_data,
	{
	  json_config, 
	  udp_id_list, 
	  ver_id_list, 
	  on_event_fn
	}).

%% These are handled by maps rather than records
-define(block, [previous_id, this_id, height, transactions, consensus_data]).
-define(consensus_block_data, [signature, verifier_pub_key, verifier_index, timestamp]).

-define(transaction, [game_data, nonce, player_id, consensus_data]).
-define(consensus_transaction_data, [signature, chain_id]).

% useful
% Trace = try throw(42) catch 42 -> erlang:get_stacktrace() end,
