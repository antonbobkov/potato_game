-module(blocktree_test).

-include_lib("eunit/include/eunit.hrl").

-include_lib("stdlib/include/assert.hrl").

-include("../src/potato_records.hrl").

get_next_nonce(Id, TD) ->
    Map = TD#tree_data.transaction_map,
    Result = maps:find(Id, Map),
    case Result of
	{ok, TransactionArray} ->
	    array:size(TransactionArray);
	error ->
	    0
    end.

add_transaction(next, Id, TD) ->
    NextNonce = get_next_nonce(Id, TD),
    add_transaction(NextNonce, Id, TD);
add_transaction(Nonce, Id, TD) ->
    T = #{nonce => Nonce, player_id => Id},
    blocktree:add_new_transaction(T, TD).

add_mult_trans({Count, _}, TD) when Count == 0 ->
    TD;
add_mult_trans({Count, Id}, TD) when Count > 0 ->
    NextNonce = get_next_nonce(Id, TD),
    {_Msg, NewTD} = add_transaction(NextNonce, Id, TD),
    %% io:format("~p ~p ~p ~n", [Id, NextNonce, Msg]),
    add_mult_trans({Count-1, Id}, NewTD).

add_new_transaction_test() ->
    TD = #tree_data{transaction_map=maps:new()},

    ?assertMatch({ignore_nonce_too_high, _}, add_transaction(1, p1, TD)),

    L = [{2, p1}, {3, p2}, {2, p1}],
    TD1 = lists:foldl(fun add_mult_trans/2, TD, L),
    %% io:format("~p ~n", [TD1]),


    MP = TD1#tree_data.transaction_map,
    ?assert(maps:size(MP) == 2),
    ?assert(array:size(maps:get(p1, MP)) == 4),
    ?assert(array:size(maps:get(p2, MP)) == 3),

    ?assertMatch({ignore_duplicate, _}, add_transaction(1, p1, TD1)),

    T = #{nonce => 1, player_id => p1, game_data => stuff},
    ?assertThrow(_, blocktree:add_new_transaction(T, TD1)),

    TD1.

add_empty_genesis_test() ->
    TD = #tree_data{transaction_map=maps:new(), block_map=maps:new()},
    Block = #{previous_id => undefined, this_id => 0, height => 0, transactions => []},
    TD1 = blocktree:add_new_block(Block, TD),

    BMP = TD1#tree_data.block_map,
    ?assert(maps:size(BMP) == 1),
    ?assert(maps:get(0, BMP) == Block),

    TD1.

map_of_arrays_to_list(Map) ->
    M = maps:map(fun (_, A) -> array:to_list(A) end, Map),
    maps:fold(fun (_, L, Acc) -> L ++ Acc end, [], M).

add_tr_genesis_test() ->
    TD = #tree_data{transaction_map=maps:new(), block_map=maps:new()},
    L = [{2, p1}, {3, p2}, {2, p1}],
    TDD = lists:foldl(fun add_mult_trans/2, TD, L),

    Block = #{previous_id => undefined, this_id => 0, height => 0,
		   transactions => map_of_arrays_to_list(TDD#tree_data.transaction_map)},
		   %% transactions = TD1#tree_data.transaction_map},
    TD1 = blocktree:add_new_block(Block, TD),

    MP = TD1#tree_data.transaction_map,
    ?assert(maps:size(MP) == 2),
    ?assert(array:size(maps:get(p1, MP)) == 4),
    ?assert(array:size(maps:get(p2, MP)) == 3),

    BMP = TD1#tree_data.block_map,
    ?assert(maps:size(BMP) == 1),
    ?assert(maps:get(0, BMP) == Block),

    TD1.

make_block(PrevId, ThisId, Height, TrLs) ->
    Fn = fun({N, Id}, Ls) ->
		 T = #{nonce => N, player_id => Id},
		 [T | Ls]
	 end,
    Transactions = lists:reverse(lists:foldl(Fn, [], TrLs)),

    #{previous_id => PrevId, this_id => ThisId, height => Height,
		   transactions  =>  Transactions}.

add_tr_genesis_2_test() ->
    TD = #tree_data{transaction_map=maps:new(), block_map=maps:new()},

    Block = make_block(undefined, hi, 0, [{0, p1}, {0, p2}, {1, p1}]),

    TD1 = blocktree:add_new_block(Block, TD),

    MP = TD1#tree_data.transaction_map,
    ?assert(maps:size(MP) == 2),
    ?assert(array:size(maps:get(p1, MP)) == 2),
    ?assert(array:size(maps:get(p2, MP)) == 1),

    BMP = TD1#tree_data.block_map,
    ?assert(maps:size(BMP) == 1),
    ?assert(maps:get(hi, BMP) == Block),

    TD1.

mult_blocks_test() ->
    TD = #tree_data{},

    Ba = make_block(undefined, a, 0, [{0, p1}, {0, p2}, {1, p1}]),
    Bb = make_block(a, b, 1, [{1, p2}, {2, p1}, {0, p3}]),
    Bc = make_block(a, c, 1, [{2, p1}, {1, p2}, {0, p4}, {2, p2}]),

    Batch = [Ba, Bb, Bc],

    TD1 = lists:foldl(fun blocktree:add_new_block/2, TD, Batch),

    MP = TD1#tree_data.transaction_map,
    ?assert(maps:size(MP) == 4),
    ?assert(array:size(maps:get(p1, MP)) == 3),
    ?assert(array:size(maps:get(p2, MP)) == 3),
    ?assert(array:size(maps:get(p3, MP)) == 1),
    ?assert(array:size(maps:get(p4, MP)) == 1),

    BMP = TD1#tree_data.block_map,
    ?assert(maps:size(BMP) == 3),

    ?assertEqual(length(blocktree:get_all_longest_branches(TD1)), 2),

    TD1.

generate_block_gen_test() ->
    TD = #tree_data{},

    L = [{2, p1}, {3, p2}, {2, p1}],
    TD1 = lists:foldl(fun add_mult_trans/2, TD, L),

    B = blocktree:generate_new_block(undefined, TD1),

    ?assert(length(maps:get(transactions, B)) == 7),

    B.


generate_block_mult_seq_test() ->
    TD = #tree_data{},

    L1 = [{2, p1}, {3, p2}],
    L2 = [{1, p1}, {3, p3}, {1, p2}],
    L3 = [{1, p2}, {1, p4}],

    Batch = [L1, L2, L3],

    Fn = fun (L, TD0) ->
		 Id = maps:size(TD0#tree_data.block_map),

		 if
		     Id == 0 -> PrevId = undefined;
		     Id /= 0 -> PrevId = Id - 1
		 end,

		 TD1 = lists:foldl(fun add_mult_trans/2, TD0, L),
		 B = (blocktree:generate_new_block(PrevId, TD1))#{this_id := Id},
		 TD2 = blocktree:add_new_block(B, TD1),
		 TD2
	 end,

    TD1 = lists:foldl(Fn, TD, Batch),

    BMP = TD1#tree_data.block_map,
    ?assert(maps:size(BMP) == 3),
    ?assert(length(maps:get(transactions, maps:get(0, BMP))) == 5),
    ?assert(length(maps:get(transactions, maps:get(1, BMP))) == 5),
    ?assert(length(maps:get(transactions, maps:get(2, BMP))) == 2),

    blocktree:get_block_by_id(0, TD1),
    blocktree:get_block_by_id(2, TD1),

    ?assertError(_, blocktree:get_block_by_id(5, TD1)),
    ?assertError(_, blocktree:get_block_by_id(hi, TD1)),

    ?assertEqual(length(blocktree:get_all_longest_branches(TD1)), 1),

    TD1.

  get_all_longest_branches_test() ->
    TD0 = #tree_data{transaction_map=maps:new(), block_map=maps:new()},
    Block0 = #{previous_id => undefined, this_id => 0, height => 0, transactions => []},
    Block1 = #{previous_id => 0, this_id => 1, height => 1, transactions => []},
    Block2 = #{previous_id => 1, this_id => 2, height => 2, transactions => []},
    %% fork from genesis
    Block3 = #{previous_id => 0, this_id => 3, height => 1, transactions => []},
    Block4 = #{previous_id => 1, this_id => 4, height => 2, transactions => []},
    TD1 = blocktree:add_new_block(Block0, TD0),
    TD2 = blocktree:add_new_block(Block1, TD1),
    TD3 = blocktree:add_new_block(Block2, TD2),
    TD4 = blocktree:add_new_block(Block3, TD3),
    TD5 = blocktree:add_new_block(Block4, TD4),
    ?assertEqual(length(blocktree:get_all_longest_branches(TD5)), 2),
    TD5.
