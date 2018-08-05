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

    {ignore_nonce_too_high, _} = add_transaction(1, p1, VD),

    L = [{2, p1}, {3, p2}, {1, p1}],
    VD1 = lists:foldl(fun add_mult_trans/2, VD, L),
    io:format("~p ~n", [VD1]),

    {ignore_duplicate, _} = add_transaction(1, p1, VD1),

    T = #transaction{nonce=1, player_id=p1, game_data=stuff},
    try blocktree:add_new_transaction(T, VD1) of
	_ -> throw("error expected, none happened")
    catch 
	throw:X -> io:format("throw ~p ~n", [X]);
	error:X -> io:format("error ~p ~n", [X]);	
	exit:X -> io:format("exit ~p ~n", [X])
    end,

    ok.
    

