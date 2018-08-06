-module(blocktree).

-export([add_new_transaction/2, add_new_block/2]).

-include("blocktree.hrl").
-include_lib("stdlib/include/assert.hrl").


add_new_transaction_to_array(Transaction, TransactionArray)
  when is_record(Transaction, transaction) ->

    Nonce = Transaction#transaction.nonce,
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
  when is_record(Transaction, transaction) ->

    Id=Transaction#transaction.player_id,
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
    
add_new_transaction(Transaction, VerifierData) 
  when is_record(Transaction, transaction),
       is_record(VerifierData, verifier_data) ->

    TransactionMap = VerifierData#verifier_data.transaction_map,

    {Msg, NewTM} = add_new_transaction_to_map(Transaction, TransactionMap),

    NewVD = VerifierData#verifier_data{transaction_map=NewTM},

    {Msg, NewVD}.

    

%% transaction_list_check_if_in_order(_, List) ->
%%     NonceList = array:map(fun (_, L) -> L#transaction.nonce end, List),
%%     FirstNonce = array:get(0, NonceList),
%%     Sz = array:size(List),
%%     ProperNonceList = lists:seq(FirstNonce, FirstNonce + Sz - 1),

%%     ?assertEqual(array:to_list(NonceList), ProperNonceList, "bad nonce order").

transaction_list_check_if_in_order(_, List) ->
    NonceList = lists:map(fun (L) -> L#transaction.nonce end, List),
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
    FirstTransaction#transaction.nonce.

get_last_nonce_in_transaction_list(TransactionList) ->
    [FirstTransaction | _ ] = TransactionList,
    FirstNonce = FirstTransaction#transaction.nonce,
    Sz = length(TransactionList),
    FirstNonce + Sz - 1.

search_previous_transaction_nonce_for_player(_, _, BlockId) when BlockId == undefined ->
    -1;
search_previous_transaction_nonce_for_player(PlayerId, BlockMap, BlockId) ->
    {ok, Block} = maps:find(BlockId, BlockMap),
    #block{previous_id=PrevBlockId, transactions=BlockTransactionsList} = Block,

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
    Id=T#transaction.player_id,
    case maps:find(Id, Map) of
	{ok, OldList} ->
	    NewList = [T | OldList];
	error ->
	    NewList = [T]
    end,
    maps:put(Id, NewList, Map).
    

add_new_block(Block, VerifierData) 
  when is_record(Block, block),
       is_record(VerifierData, verifier_data) ->

    #verifier_data{block_map = BlockMap} = VerifierData,
    #block{previous_id=PrevId, this_id=ThisId, height=Height, transactions=BlockTransactionsList} = Block,

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

	    ?assertEqual(Height, PrevBlock#block.height + 1, "bad height"),

	    MapFn = fun(PlayerId, _) -> 1 + search_previous_transaction_nonce_for_player(PlayerId, BlockMap, PrevId) end,
	    FirstNonceMapProper = maps:map(MapFn, BlockTransactionsMap),

	    ?assertEqual(FirstNonceMap, FirstNonceMapProper, "transactions not starting with correct nonce")
    end,

    VD0 = VerifierData,

    NewBlockMap = maps:put(ThisId, Block, BlockMap),

    VD1 = VD0#verifier_data{block_map = NewBlockMap},

    ListFoldFn = fun(T, VD) -> {_, NewVD} = add_new_transaction(T, VD), NewVD end,
    MapFoldFn = fun(_, TransactionList, VD) -> lists:foldl(ListFoldFn, VD, TransactionList) end,

    VD2 = maps:fold(MapFoldFn, VD1, BlockTransactionsMap),

    VD2.
