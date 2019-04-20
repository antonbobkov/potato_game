%% @doc Blocktree
%% 
%% Potential optimization: searches for previous nonce by player can 
%% be made faster by caching previous searches. Would speed up
%% add_block_in_order and generate_new_block

-module(blocktree).

-export([
	 new/0,
	 add_new_transaction/2,
	 add_block_in_order/2,
	 generate_new_block/2,
	 get_block_by_id/2,
	 get_status_info/1
	 %% get_all_longest_branches/1,
	 %% get_children_block_list/2
	]).

-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

-type tx() :: map().
%% -type txarray() :: array:array(tx()).
-type txlist() :: [tx()].
-type playertxmap() :: map().
-type addresult() :: ignored_duplicate | updated_old | added_new.
%% -type addresult() :: any().
-type treedata() :: #tree_data{}.
-type playerid() :: integer().
-type nonce() :: integer().
-type blockid() :: integer() | undefined.
-type blockmap() :: map().
-type block() :: map().

%% @doc Initializes an empty container
-spec new() -> treedata().
new() ->
    #tree_data{
       pending_transactions = pending_transactions:new(), 
       block_map = maps:new()
      }.

%% @doc Adds a new transaction to transaction list.
%%
%% Called when we get a new transaction from a player.
%% Can do it out of order.
%% Duplicate transactions are ignored, but we can rewrite a transaction 
%% with a given nonce by a different transaction with the same nonce.
%% Returns {Status, Container} where Status is 
%% ignored_duplicate, updated_old, added_new

-spec add_new_transaction(tx(), treedata()) -> {addresult(), treedata()}.
add_new_transaction(Transaction, TreeData)
  when is_map(Transaction),
       is_record(TreeData, tree_data) ->

    {PendingTxNew, Status} = pending_transactions:add_transaction(Transaction, TreeData#tree_data.pending_transactions),

    {Status, TreeData#tree_data{pending_transactions = PendingTxNew}}.
    

-spec transaction_list_check_if_in_order(playerid(), txlist()) -> ok.
transaction_list_check_if_in_order(_, List) ->
    NonceList = lists:map(fun (T) -> maps:get(nonce, T) end, List),
    [FirstNonce | _] = NonceList,
    Sz = length(List),
    ProperNonceList = lists:seq(FirstNonce, FirstNonce + Sz - 1),

    ?assertEqual(NonceList, ProperNonceList, "bad nonce order").

-spec get_first_nonce_in_transaction_list(playerid(), txlist()) -> nonce().
get_first_nonce_in_transaction_list(_, TransactionList) ->
    [FirstTransaction | _ ] = TransactionList,
    maps:get(nonce, FirstTransaction).

-spec get_last_nonce_in_transaction_list(txlist()) -> nonce().
get_last_nonce_in_transaction_list(TransactionList) ->
    [FirstTransaction | _ ] = TransactionList,
    FirstNonce = maps:get(nonce, FirstTransaction),
    Sz = length(TransactionList),
    FirstNonce + Sz - 1.

-spec search_previous_transaction_nonce_for_player(playerid(), blockmap(), blockid()) -> nonce().
search_previous_transaction_nonce_for_player(_, _, BlockId) when BlockId == undefined ->
    -1;
search_previous_transaction_nonce_for_player(PlayerId, BlockMap, BlockId) ->
    {ok, Block} = maps:find(BlockId, BlockMap),
    #{previous_id := PrevBlockId, transactions := BlockTransactionsList} = Block,

    BlockTransactionsMap = transaction_map_from_list(BlockTransactionsList),

    case maps:find(PlayerId, BlockTransactionsMap) of
	{ok, TransactionList} ->
	    get_last_nonce_in_transaction_list(TransactionList);
	error ->
	    search_previous_transaction_nonce_for_player(PlayerId, BlockMap, PrevBlockId)
    end.

-spec transaction_map_from_list(txlist()) -> playertxmap().
transaction_map_from_list(ListR) ->
    List = lists:reverse(ListR),
    lists:foldl(fun transaction_map_from_list/2, maps:new(), List).

transaction_map_from_list(T, Map) ->
    Id = maps:get(player_id, T),

    case maps:find(Id, Map) of
	{ok, OldList} ->
	    NewList = [T | OldList];
	error ->
	    NewList = [T]
    end,
    maps:put(Id, NewList, Map).

%% @doc Adds a new block to the blocktree.
%%
%% If successful, it returns updated TreeData.
%%
%% Fails if:
%%
%% <ul>
%% <li> can't find previous block</li>
%% <li> this_id is not unique </li>
%% <li> height is incorrect</li>
%% <li> transactions list is incorrect:
%%   <ul>
%%     <li> Nonces in each list should be in order</li>
%%     <li> First nonce in a list should come after the last nonce in the chain for that player</li>
%%   </ul>
%% </li>
%% </ul>
%%
%% Note: genesis block will have previous_id=undefined and height=0



-spec add_block_in_order(block(), treedata()) -> treedata().
add_block_in_order(Block, TreeData)
  when is_map(Block),
       is_record(TreeData, tree_data) ->

    #tree_data{block_map = BlockMap} = TreeData,
    #{previous_id := PrevId, this_id := ThisId, height := Height, transactions := BlockTransactionsList} = Block,

    ?assertEqual(error, maps:find(ThisId, BlockMap), "this_id already exists"),

    BlockTransactionsMap = transaction_map_from_list(BlockTransactionsList),

    maps:map(fun transaction_list_check_if_in_order/2, BlockTransactionsMap),

    %% check_that_player_ids_are_correct(BlockTransactionsMap),

    FirstNonceMap = maps:map(fun get_first_nonce_in_transaction_list/2, BlockTransactionsMap),

    MapEmpty = maps:size(BlockMap) == 0,
    if
	MapEmpty ->
	    ?assertEqual(Height, 0, "genesis, bad height"),

	    ?assertEqual(PrevId, undefined, "genesis, bad previous_id"),

	    ZeroNonceMap = maps:map(fun(_, _) -> 0 end, BlockTransactionsMap),

	    ?assertEqual(FirstNonceMap,  ZeroNonceMap, "genesis, transactions not starting with zero");

	not MapEmpty ->
	    Result = maps:find(PrevId, BlockMap),

	    ?assertMatch({ok, _}, Result, "cannot find previous_id"),

	    {ok, PrevBlock} = Result,

	    ?assertEqual(Height, maps:get(height, PrevBlock) + 1, "bad height"),

	    MapFn = fun(PlayerId, _) -> 1 + search_previous_transaction_nonce_for_player(PlayerId, BlockMap, PrevId) end,
	    FirstNonceMapProper = maps:map(MapFn, BlockTransactionsMap),

	    ?assertEqual(FirstNonceMap, FirstNonceMapProper, "transactions not starting with correct nonce")
    end,

    NewBlockMap = maps:put(ThisId, Block, BlockMap),

    TreeData#tree_data{block_map = NewBlockMap}.

%% @doc Generates new block after block with PreviousBlockId.
%% 
%% Puts the appropriate pending transactions inside.

-spec generate_new_block(blockid(), treedata()) -> map().
generate_new_block(PreviousBlockId, TreeData)
  when is_record(TreeData, tree_data) ->
    #tree_data{
       block_map = BlockMap,
       pending_transactions = PendingTx
      } = TreeData,

    if
	PreviousBlockId == undefined ->
	    Height = 0;

	PreviousBlockId /= undefined ->
	    Result = maps:find(PreviousBlockId, BlockMap),

	    ?assertMatch({ok, _}, Result, "cannot find previous_id"),

	    {ok, PrevBlock} = Result,

	    Height = 1 + maps:get(height, PrevBlock)
    end,
    
    PendingPlayers = pending_transactions:get_pending_players(PendingTx),

    MapFn = fun(PlayerId) -> 
		    NextNonce = 1 + search_previous_transaction_nonce_for_player(PlayerId, BlockMap, PreviousBlockId),
		    {PlayerId, NextNonce}
	    end,

    FirstNonceMap = maps:from_list(lists:map(MapFn, PendingPlayers)),

    BlockTransactions = pending_transactions:get_pending_transactions(FirstNonceMap, PendingTx),

    #{
      previous_id => PreviousBlockId,
      this_id => undefined,
      height => Height,
      transactions => BlockTransactions,
      consensus_data => undefined
     }.

-spec get_block_by_id(blockid(), treedata()) -> block().
get_block_by_id(Id, TreeData)
  when is_record(TreeData, tree_data) ->

    #tree_data{block_map = BlockMap} = TreeData,
    Result = maps:find(Id, BlockMap),

    ?assertMatch({ok, _}, Result, "cannot find block by id"),
    {ok, Block} = Result,
    Block.

%% -spec get_all_longest_branches(treedata()) -> [block()].
%% get_all_longest_branches(TreeData)
%%   when is_record(TreeData, tree_data) ->
%%     #tree_data{block_map = BlockMap} = TreeData,
%%     MaxHtFn = fun(_, V, Max) -> max(maps:get(height, V), Max) end,
%%     MaxHt = maps:fold(MaxHtFn, 0, BlockMap),
%%     MaxHtList = maps:values(maps:filter(fun(_, V) -> maps:get(height, V) == MaxHt end, BlockMap)),
%%     MaxHtList.

%% -spec get_children_block_list(blockid(), treedata()) -> [block()].
%% get_children_block_list(PrevId, TreeData)
%%   when is_record(TreeData, tree_data) ->
%%     #tree_data{block_map = BlockMap} = TreeData,
%%     maps:values(maps:filter(fun(_, B) -> maps:get(previous_id, B) == PrevId end, BlockMap)).


get_status_info(_TreeData = #tree_data{pending_transactions = Tx, block_map = Mp}) ->
    
    pending_transactions:get_status_info(Tx) ++
    [
     {"blocks in tree", maps:size(Mp)}
    ].
    

    
    
