-module(blocktree).

-export([
	 add_new_transaction/2,
	 add_new_block/2,
	 generate_new_block/2,
	 get_block_by_id/2,
	 get_all_longest_branches/1,
	 get_children_block_list/2
	]).

-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").



add_new_transaction_to_array(Transaction, TransactionArray)
  when is_map(Transaction) ->

    Nonce = maps:get(nonce, Transaction),
    TA = TransactionArray,
    ArrSz = array:size(TA),

    if
	Nonce > ArrSz ->
	    {ignore_nonce_too_high, TA};
	Nonce < ArrSz ->
	    RecordedTransaction = array:get(Nonce, TA),
	    if
		RecordedTransaction == Transaction ->
		    {ignore_duplicate, TA};
		RecordedTransaction /= Transaction ->
		    throw("same_nonce_different_transaction")
	    end;
	Nonce == ArrSz ->
	    NewTA = array:set(Nonce, Transaction, TA),
	    {added, NewTA}
    end.

add_new_transaction_to_map(Transaction, TransactionMap)
  when is_map(Transaction) ->

    Id = maps:get(player_id, Transaction),
    TM = TransactionMap,
    Result = maps:find(Id, TM),

    case Result of
	{ok, TransactionArray} ->
	    pass;
	error ->
	    TransactionArray = array:new()
    end,

    {Msg, NewTA} = add_new_transaction_to_array(Transaction, TransactionArray),
    NewTM = maps:put(Id, NewTA, TM),
    {Msg, NewTM}.

%% @doc Adds a new transaction to transaction list.
%% 
%% Can only do so in order.
%% High nonce, and duplicate transactions are ignored.
%% Fails with error if there is a different transaction with the same nonce.
%% This function will be called indirectly from add_new_block

add_new_transaction(Transaction, TreeData)
  when is_map(Transaction),
       is_record(TreeData, tree_data) ->

    TransactionMap = TreeData#tree_data.transaction_map,

    {Msg, NewTM} = add_new_transaction_to_map(Transaction, TransactionMap),

    NewTD = TreeData#tree_data{transaction_map=NewTM},

    {Msg, NewTD}.



%% transaction_list_check_if_in_order(_, List) ->
%%     NonceList = array:map(fun (_, L) -> L#transaction.nonce end, List),
%%     FirstNonce = array:get(0, NonceList),
%%     Sz = array:size(List),
%%     ProperNonceList = lists:seq(FirstNonce, FirstNonce + Sz - 1),

%%     ?assertEqual(array:to_list(NonceList), ProperNonceList, "bad nonce order").

transaction_list_check_if_in_order(_, List) ->
    NonceList = lists:map(fun (T) -> maps:get(nonce, T) end, List),
    [FirstNonce | _] = NonceList,
    Sz = length(List),
    ProperNonceList = lists:seq(FirstNonce, FirstNonce + Sz - 1),

    ?assertEqual(NonceList, ProperNonceList, "bad nonce order").

%% get_first_nonce_in_transaction_list(_, TransactionList) ->
%%     FirstTransaction = array:get(0, TransactionList),
%%     FirstTransaction#transaction.nonce.

%% get_last_nonce_in_transaction_list(TransactionList) ->
%%     FirstTransaction = array:get(0, TransactionList),
%%     FirstNonce = FirstTransaction#transaction.nonce,
%%     Sz = array:size(TransactionList),
%%     FirstNonce + Sz - 1.

get_first_nonce_in_transaction_list(_, TransactionList) ->
    [FirstTransaction | _ ] = TransactionList,
    maps:get(nonce, FirstTransaction).

get_last_nonce_in_transaction_list(TransactionList) ->
    [FirstTransaction | _ ] = TransactionList,
    FirstNonce = maps:get(nonce, FirstTransaction),
    Sz = length(TransactionList),
    FirstNonce + Sz - 1.

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

%% check_that_player_ids_are_correct(id, IdCheck, IdCorrect) when IdCheck == IdCorrect ->
%%     ok;
%% check_that_player_ids_are_correct(id, IdCheck, IdCorrect) when IdCheck /= IdCorrect ->
%%     throw("bad player_id in BlockTransactions");
%% check_that_player_ids_are_correct(transaction, Transaction, IdCorrect) ->
%%     IdCheck=Transaction#transaction.player_id,
%%     check_that_player_ids_are_correct(id, IdCheck, IdCorrect);
%% check_that_player_ids_are_correct(list, TransactionList, IdCorrect) ->
%%     lists:map(fun(T) -> check_that_player_ids_are_correct(transaction, T, IdCorrect) end, TransactionList).
%% check_that_player_ids_are_correct(TransactionMap) ->
%%     maps:map(fun(Id, Lst) -> check_that_player_ids_are_correct(list, Lst, Id) end, TransactionMap).

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


add_new_block(Block, TreeData)
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

    TD0 = TreeData,

    NewBlockMap = maps:put(ThisId, Block, BlockMap),

    TD1 = TD0#tree_data{block_map = NewBlockMap},

    ListFoldFn = fun(T, TD) -> {_, NewTD} = add_new_transaction(T, TD), NewTD end,
    MapFoldFn = fun(_, TransactionList, TD) -> lists:foldl(ListFoldFn, TD, TransactionList) end,

    TD2 = maps:fold(MapFoldFn, TD1, BlockTransactionsMap),

    TD2.

extract_transaction_range(NonceFirst, FullTransactionArr) ->
    extract_transaction_range(NonceFirst, array:size(FullTransactionArr)-1, FullTransactionArr).

extract_transaction_range(NonceFirst, NonceLast, FullTransactionArr) when NonceFirst =< NonceLast ->
    T = array:get(NonceFirst, FullTransactionArr),
    [T | extract_transaction_range(NonceFirst + 1, NonceLast, FullTransactionArr)];

extract_transaction_range(NonceFirst, NonceLast, _) when NonceFirst > NonceLast ->
    [].

extract_transaction_range_full(FirstNonceMap, TransactionMap) ->
    Fn = fun(Id, _, Acc) ->
		 {ok, Nonce} = maps:find(Id, FirstNonceMap),
		 {ok, Arr} = maps:find(Id, TransactionMap),
		 L = extract_transaction_range(Nonce, Arr),
		 L ++ Acc
	 end,
    maps:fold(Fn, [], FirstNonceMap).


generate_new_block(PreviousBlockId, TreeData)
  when is_record(TreeData, tree_data) ->
    #tree_data{block_map = BlockMap, transaction_map = TransactionMap} = TreeData,

    if
	PreviousBlockId == undefined ->
	    Height = 0;

	PreviousBlockId /= undefined ->
	    Result = maps:find(PreviousBlockId, BlockMap),

	    ?assertMatch({ok, _}, Result, "cannot find previous_id"),

	    {ok, PrevBlock} = Result,

	    Height = 1 + maps:get(height, PrevBlock)
    end,


    MapFn = fun(PlayerId, _) -> 1 + search_previous_transaction_nonce_for_player(PlayerId, BlockMap, PreviousBlockId) end,
    FirstNonceMap = maps:map(MapFn, TransactionMap),

    BlockTransactions = extract_transaction_range_full(FirstNonceMap, TransactionMap),

    #{
      previous_id => PreviousBlockId,
      this_id => undefined,
      height => Height,
      transactions => BlockTransactions,
      consensus_data => undefined
     }.


get_block_by_id(Id, TreeData)
  when is_record(TreeData, tree_data) ->

    #tree_data{block_map = BlockMap} = TreeData,
    Result = maps:find(Id, BlockMap),

    ?assertMatch({ok, _}, Result, "cannot find block by id"),
    {ok, Block} = Result,
    Block.

get_all_longest_branches(TreeData)
  when is_record(TreeData, tree_data) ->
    #tree_data{block_map = BlockMap} = TreeData,
    MaxHtFn = fun(_, V, Max) -> max(maps:get(height, V), Max) end,
    MaxHt = maps:fold(MaxHtFn, 0, BlockMap),
    MaxHtList = maps:values(maps:filter(fun(_, V) -> maps:get(height, V) == MaxHt end, BlockMap)),
    MaxHtList.

get_children_block_list(PrevId, TreeData)
  when is_record(TreeData, tree_data) ->
    #tree_data{block_map = BlockMap} = TreeData,
    maps:values(maps:filter(fun(_, B) -> maps:get(previous_id, B) == PrevId end, BlockMap)).
    