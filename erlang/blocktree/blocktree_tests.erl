-module(blocktree_tests).
-export([cmd/0]).

-include_lib("stdlib/include/assert.hrl").

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
    {_Msg, NewVD} = add_transaction(NextNonce, Id, VD),
    %% io:format("~p ~p ~p ~n", [Id, NextNonce, Msg]),
    add_mult_trans({Count-1, Id}, NewVD).

test_add_new_transaction() ->
    VD = #verifier_data{transaction_map=maps:new()},

    ?assertMatch({ignore_nonce_too_high, _}, add_transaction(1, p1, VD)),

    L = [{2, p1}, {3, p2}, {2, p1}],
    VD1 = lists:foldl(fun add_mult_trans/2, VD, L),
    %% io:format("~p ~n", [VD1]),


    MP = VD1#verifier_data.transaction_map,
    ?assert(maps:size(MP) == 2),
    ?assert(array:size(maps:get(p1, MP)) == 4),
    ?assert(array:size(maps:get(p2, MP)) == 3),

    ?assertMatch({ignore_duplicate, _}, add_transaction(1, p1, VD1)),

    T = #transaction{nonce=1, player_id=p1, game_data=stuff},
    ?assertThrow(_, blocktree:add_new_transaction(T, VD1)),

    VD1.

test_add_empty_genesis() ->
    VD = #verifier_data{transaction_map=maps:new(), block_map=maps:new()},
    Block = #block{previous_id=undefined, this_id=0, height=0, transactions=[]},
    VD1 = blocktree:add_new_block(Block, VD),

    BMP = VD1#verifier_data.block_map,
    ?assert(maps:size(BMP) == 1),
    ?assert(maps:get(0, BMP) == Block),

    VD1.

map_of_arrays_to_list(Map) ->
    M = maps:map(fun (_, A) -> array:to_list(A) end, Map),
    maps:fold(fun (_, L, Acc) -> L ++ Acc end, [], M).
    
test_add_tr_genesis() ->
    VD = #verifier_data{transaction_map=maps:new(), block_map=maps:new()},
    L = [{2, p1}, {3, p2}, {2, p1}],
    VDD = lists:foldl(fun add_mult_trans/2, VD, L),

    Block = #block{previous_id=undefined, this_id=0, height=0, 
		   transactions = map_of_arrays_to_list(VDD#verifier_data.transaction_map)},
		   %% transactions = VD1#verifier_data.transaction_map},
    VD1 = blocktree:add_new_block(Block, VD),

    MP = VD1#verifier_data.transaction_map,
    ?assert(maps:size(MP) == 2),
    ?assert(array:size(maps:get(p1, MP)) == 4),
    ?assert(array:size(maps:get(p2, MP)) == 3),    

    BMP = VD1#verifier_data.block_map,
    ?assert(maps:size(BMP) == 1),
    ?assert(maps:get(0, BMP) == Block),

    VD1.

make_block(PrevId, ThisId, Height, TrLs) ->
    Fn = fun({N, Id}, Ls) ->
		 T = #transaction{nonce=N, player_id=Id},
		 [T | Ls]
	 end,
    Transactions = lists:reverse(lists:foldl(Fn, [], TrLs)),

    #block{previous_id=PrevId, this_id=ThisId, height=Height, 
		   transactions = Transactions}.

test_add_tr_genesis_2() ->
    VD = #verifier_data{transaction_map=maps:new(), block_map=maps:new()},

    Block = make_block(undefined, hi, 0, [{0, p1}, {0, p2}, {1, p1}]),

    VD1 = blocktree:add_new_block(Block, VD),

    MP = VD1#verifier_data.transaction_map,
    ?assert(maps:size(MP) == 2),
    ?assert(array:size(maps:get(p1, MP)) == 2),
    ?assert(array:size(maps:get(p2, MP)) == 1),    

    BMP = VD1#verifier_data.block_map,
    ?assert(maps:size(BMP) == 1),
    ?assert(maps:get(hi, BMP) == Block),

    VD1.

test_mult_blocks() ->
    VD = #verifier_data{},

    Ba = make_block(undefined, a, 0, [{0, p1}, {0, p2}, {1, p1}]),
    Bb = make_block(a, b, 1, [{1, p2}, {2, p1}, {0, p3}]),
    Bc = make_block(a, c, 1, [{2, p1}, {1, p2}, {0, p4}, {2, p2}]),

    Batch = [Ba, Bb, Bc],

    VD1 = lists:foldl(fun blocktree:add_new_block/2, VD, Batch),

    MP = VD1#verifier_data.transaction_map,
    ?assert(maps:size(MP) == 4),
    ?assert(array:size(maps:get(p1, MP)) == 3),
    ?assert(array:size(maps:get(p2, MP)) == 3),    
    ?assert(array:size(maps:get(p3, MP)) == 1),
    ?assert(array:size(maps:get(p4, MP)) == 1),    

    BMP = VD1#verifier_data.block_map,
    ?assert(maps:size(BMP) == 3),

    VD1.

cmd() ->
    test_add_new_transaction(),
    test_add_empty_genesis(),
    test_add_tr_genesis(),
    test_add_tr_genesis_2(),
    test_mult_blocks().
