-module(blocktree_test).

-include_lib("eunit/include/eunit.hrl").

-include_lib("stdlib/include/assert.hrl").

-include("../src/potato_records.hrl").


add_transaction({ [Nonce | NonceT], Id}, TD) ->
    TD1 = add_transaction({Nonce, Id}, TD),
    add_transaction({NonceT, Id}, TD1);

add_transaction({ [], _Id}, TD) -> TD;

add_transaction({Nonce, Id}, TD) ->
    T = #{nonce => Nonce, player_id => Id},
    {_, TD1} = blocktree:add_new_transaction(T, TD),
    TD1;

add_transaction({Nonce, Id, Data}, TD) ->
    T = #{nonce => Nonce, player_id => Id, game_data => Data},
    {_, TD1} = blocktree:add_new_transaction(T, TD),
    TD1.

add_many_transactions(List, TD) ->
    lists:foldl(fun add_transaction/2, TD, List).

get_block_transactions(B) ->
    TxList = maps:get(transactions, B),
    lists:map(fun(T) -> {maps:get(nonce, T), maps:get(player_id, T)} end, TxList).


add_new_transaction_test() ->
    TD = blocktree:new(),

    T1 = #{nonce => 0, player_id => p1},
    TT1 = #{nonce => 0, player_id => p1, game_data => hi},

    {S, TD1} = blocktree:add_new_transaction(T1, TD),
    ?assertEqual(S, added_new),

    ?assertMatch({ignored_duplicate, _}, blocktree:add_new_transaction(T1, TD1)),
    ?assertMatch({updated_old, _}, blocktree:add_new_transaction(TT1, TD1)),
    
    ok.

add_empty_genesis_test() ->
    TD = blocktree:new(),
    Block = #{previous_id => undefined, this_id => 0, height => 0, transactions => []},
    TD1 = blocktree:add_block_in_order(Block, TD),

    BMP = TD1#tree_data.block_map,
    ?assert(maps:size(BMP) == 1),
    ?assert(maps:get(0, BMP) == Block),

    TD1.

%% map_of_arrays_to_list(Map) ->
%%     M = maps:map(fun (_, A) -> array:to_list(A) end, Map),
%%     maps:fold(fun (_, L, Acc) -> L ++ Acc end, [], M).

add_tr_genesis_test() ->
    TD = blocktree:new(),

    Block = #{previous_id => undefined, this_id => 0, height => 0,  transactions => []},
    TD1 = blocktree:add_block_in_order(Block, TD),


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
    TD = blocktree:new(),

    Block = make_block(undefined, hi, 0, [{0, p1}, {0, p2}, {1, p1}]),

    TD1 = blocktree:add_block_in_order(Block, TD),

    %% MP = TD1#tree_data.transaction_map,
    %% ?assert(maps:size(MP) == 2),
    %% ?assert(array:size(maps:get(p1, MP)) == 2),
    %% ?assert(array:size(maps:get(p2, MP)) == 1),

    BMP = TD1#tree_data.block_map,
    ?assert(maps:size(BMP) == 1),
    ?assert(maps:get(hi, BMP) == Block),

    TD1.

mult_blocks_test() ->
    TD = blocktree:new(),

    Ba = make_block(undefined, a, 0, [{0, p1}, {0, p2}, {1, p1}]),
    Bb = make_block(a, b, 1, [{1, p2}, {2, p1}, {0, p3}]),
    Bc = make_block(a, c, 1, [{2, p1}, {1, p2}, {0, p4}, {2, p2}]),

    Batch = [Ba, Bb, Bc],

    TD1 = lists:foldl(fun blocktree:add_block_in_order/2, TD, Batch),

    %% MP = TD1#tree_data.transaction_map,
    %% ?assert(maps:size(MP) == 4),
    %% ?assert(array:size(maps:get(p1, MP)) == 3),
    %% ?assert(array:size(maps:get(p2, MP)) == 3),
    %% ?assert(array:size(maps:get(p3, MP)) == 1),
    %% ?assert(array:size(maps:get(p4, MP)) == 1),

    BMP = TD1#tree_data.block_map,
    ?assert(maps:size(BMP) == 3),

    %% ?assertEqual(length(blocktree:get_all_longest_branches(TD1)), 2),

    TD1.


generate_block_gen_test() ->
    TD0 = blocktree:new(),

    TD1 = add_many_transactions([
				 {0, p1}, {2, p1}, {3, p1},
				 {0, p2}, {1, p2}, {2, p2}
				], TD0),

    B0 = (blocktree:generate_new_block(undefined, TD1))#{this_id := 0},

    ?assertEqual([ {0, p1}, {0, p2}, {1, p2}, {2, p2} ], get_block_transactions(B0)),

    TD2 = blocktree:add_block_in_order(B0, TD1),

    TD3 = add_many_transactions([ {3, p2}, {1, p1}, {5, p2} ], TD2),

    B1 = (blocktree:generate_new_block(0, TD3))#{this_id := 1},

    ?assertEqual([ {3, p2}, {1, p1}, {2, p1}, {3, p1} ], get_block_transactions(B1)),
    ok.


generate_block_mult_seq_test() ->
    TD = blocktree:new(),

    L1 = [
	  {[0,1], p1}, 
	  {[0,1,2], p2}
	 ],

    L2 = [
	  {2, p1}, 
	  {[0, 1, 2], p3}, 
	  {3, p2}
	 ],

    L3 = [
	  {4, p2}, 
	  {0, p4}
	 ],

    Batch = [L1, L2, L3],

    Fn = fun (L, TD0) ->
		 Id = maps:size(TD0#tree_data.block_map),

		 if
		     Id == 0 -> PrevId = undefined;
		     Id /= 0 -> PrevId = Id - 1
		 end,

		 TD1 = add_many_transactions(L, TD0),
		 B = (blocktree:generate_new_block(PrevId, TD1))#{this_id := Id},
		 TD2 = blocktree:add_block_in_order(B, TD1),
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

    %% ?assertEqual(length(blocktree:get_all_longest_branches(TD1)), 1),

    TD1.

  %% get_all_longest_branches_test() ->
  %%   TD0 = #tree_data{transaction_map=maps:new(), block_map=maps:new()},
  %%   Block0 = #{previous_id => undefined, this_id => 0, height => 0, transactions => []},
  %%   Block1 = #{previous_id => 0, this_id => 1, height => 1, transactions => []},
  %%   Block2 = #{previous_id => 1, this_id => 2, height => 2, transactions => []},
  %%   %% fork from genesis
  %%   Block3 = #{previous_id => 0, this_id => 3, height => 1, transactions => []},
  %%   Block4 = #{previous_id => 1, this_id => 4, height => 2, transactions => []},
  %%   TD1 = blocktree:add_block_in_order(Block0, TD0),
  %%   TD2 = blocktree:add_block_in_order(Block1, TD1),
  %%   TD3 = blocktree:add_block_in_order(Block2, TD2),
  %%   TD4 = blocktree:add_block_in_order(Block3, TD3),
  %%   TD5 = blocktree:add_block_in_order(Block4, TD4),

  %%   ?assertEqual(length(blocktree:get_all_longest_branches(TD5)), 2),

    
  %%   ?assertEqual(length(blocktree:get_children_block_list(0, TD5)), 2),
  %%   ?assertEqual(length(blocktree:get_children_block_list(0, TD3)), 1),
  %%   ?assertEqual(length(blocktree:get_children_block_list(1, TD5)), 2),
  %%   ?assertEqual(length(blocktree:get_children_block_list(2, TD5)), 0),

  %%   TD5.

%% Lists search is not defined for our version of erlang ಠ_ಠ
%% lists_search_test() ->
%%     lists:search(fun(_) -> true end, []).
