-module(blocktree).
-export([add_new_transaction/2]).

-include("blocktree.hrl").

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
		    error("same_nonce_different_transaction")
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

    {NewTM, Msg} = add_new_transaction_to_map(Transaction, TransactionMap),

    NewVD = VerifierData#verifier_data{transaction_map=NewTM},

    {NewVD, Msg}.
    

	    
    
