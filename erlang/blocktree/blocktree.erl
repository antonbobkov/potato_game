-module(blocktree).
-export([add_new_transaction/2]).

-record(transaction, {game_data, nonce, player_id, consensus_data}).

add_new_transaction_to_array(TransactionArray, #transaction{nonce=Nonce}=Transaction) ->
    TA = TransactionArray,
    ArrSz = array:size(TransactionArray),
    if
	Nonce > ArrSz ->
	    {TA, ignore_nonce_too_high};
	Nonce < ArrSz ->
	    RecordedTransaction = array:get(Nonce, TA),
	    if 
		RecordedTransaction == Transaction ->
		    {TA, ignore_duplicate};
		RecordedTransaction /= Transaction ->
		    {TA, error_same_nonce_different_transaction}
	    end;
	Nonce == ArrSz ->
	    {array:set(Nonce, Transaction, TA), added}
    end.
	    
add_new_transaction_to_map(TransactionMap, #transaction{player_id=Id}=Transaction) ->
    Result = maps:find(Id, TransactionMap),
    case Result of
	{ok, TransactionArray} ->
	    Msg2 = existing_player;
	error ->
	    TransactionArray = array:new(),
	    Msg2 = new_player
    end,

    {NewTA, Msg} = add_new_transaction_to_array(TransactionArray, Transaction),
    NewTM = maps:put(Id, NewTA, TransactionMap),
    {NewTM, Msg, Msg2}.
    
add_new_transaction(VerifierData, Transaction) ->
    {B, TransactionMap} = VerifierData,
    {NewTM, Msg1, Msg2} = add_new_transaction_to_map(TransactionMap, Transaction),
    {{B, NewTM}, Msg1, Msg2}.
    

	    
    
