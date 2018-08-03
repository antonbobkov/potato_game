-module(blocktree).
-export([add_new_transaction/2]).

-record(transaction, {game_data, nonce, player_id, consensus_data}).

add_new_transaction_to_array(TransactionArray, #transaction{nonce=Nonce}=Transaction) ->
    TA = TransactionArray,
    ArrSz = array:size(TA),
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
	    NewTA = array:set(Nonce, Transaction, TA),
	    {NewTA, added}
    end.
	    
add_new_transaction_to_map(TransactionMap, #transaction{player_id=Id}=Transaction) ->
    TM = TransactionMap,
    Result = maps:find(Id, TM),
    case Result of
	{ok, TransactionArray} ->
	    Msg2 = existing_player;
	error ->
	    TransactionArray = array:new(),
	    Msg2 = new_player
    end,

    {NewTA, Msg} = add_new_transaction_to_array(TransactionArray, Transaction),
    NewTM = maps:put(Id, NewTA, TM),
    {NewTM, Msg, Msg2}.
    
add_new_transaction(VerifierData, Transaction) ->
    VD = VerifierData,
    {B, TransactionMap} = VD,
    {NewTM, Msg1, Msg2} = add_new_transaction_to_map(TransactionMap, Transaction),
    NewVD = {B, NewTM},
    {NewVD, Msg1, Msg2}.
    

	    
    
