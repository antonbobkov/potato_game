-module(blocktree_tests).
-export([cmd/0]).

-include("blocktree.hrl").

get_next_nonce(Id, VD) ->
    Map = VD#verifier_data.transaction_map,
    Result = maps:find(Id, Map),
    case Result of
	{ok, TransactionArray} ->
	    array:size(TransactionArray);
	error ->
	    0
    end.
    
add_transaction(next, Id, VD) ->
    NextNonce = get_next_nonce(Id, VD),
    add_transaction(NextNonce, Id, VD); 
add_transaction(Nonce, Id, VD) ->
    T = #transaction{nonce=Nonce, player_id=Id},
    blocktree:add_new_transaction(T, VD).

add_mult_trans({Count, _}, VD) when Count == 0 ->
    VD;
add_mult_trans({Count, Id}, VD) when Count > 0 ->
    NextNonce = get_next_nonce(Id, VD),
    {Msg, NewVD} = add_transaction(NextNonce, Id, VD),
    io:format("~p ~p ~p ~n", [Id, NextNonce, Msg]),
    add_mult_trans({Count-1, Id}, NewVD).

   
    

cmd() ->
    VD = #verifier_data{transaction_map=maps:new()},
    %% add_transaction(0, p1, VD).
    %% add_mult_trans(10, p1, VD).
    L = [{10, p1}, {10, p2}],
    lists:foldl(fun add_mult_trans/2, VD, L).
    

