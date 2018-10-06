-module(pending_transactions_test).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../src/potato_records.hrl").

fold_transactions(T, {PendingTx, _Status}) when is_map(T) ->
    pending_transactions:add_transaction(T, PendingTx);
fold_transactions({Nonce, Id}, Acc) ->
    fold_transactions(#{nonce => Nonce, player_id => Id}, Acc);
fold_transactions({Nonce, Id, Data}, Acc) ->
    fold_transactions(#{nonce => Nonce, player_id => Id, game_data => Data}, Acc);
fold_transactions(Nonce, Acc) ->
    fold_transactions(#{nonce => Nonce, player_id => default_id}, Acc).

apply_transactions(TxList, PendingTx) ->
    lists:foldl(fun fold_transactions/2, {PendingTx, dummy}, TxList).

apply_transactions(TxList) -> apply_transactions(TxList, pending_transactions:init()).

get_nonce_counter_list(PlayerId, #pending_tx{player_map = PlMap}) ->
    TxMap = maps:get(PlayerId, PlMap),
    NonceList = lists:sort(maps:keys(TxMap)), 
    lists:map(fun(Nonce) -> 
		      {Counter, T} = maps:get(Nonce, TxMap),
		      Nonce = maps:get(nonce, T),
		      {Nonce, Counter} 
	      end, 
	      NonceList).

get_nonce_counter_list(PT) -> get_nonce_counter_list(default_id, PT).

get_nonce_list(PlayerId, PT) ->
    lists:map(fun({Nonce, _Counter}) -> Nonce end, get_nonce_counter_list(PlayerId, PT)).
    
get_nonce_list(PT) -> get_nonce_list(default_id, PT).

get_final_nonces(List) ->
    {PT1, _} = apply_transactions(List),
    get_nonce_list(PT1).

get_final_nonces_counters(List) ->
    {PT1, _} = apply_transactions(List),
    get_nonce_counter_list(PT1).

no_repeats(List) ->
    array:to_list(
      array:map(
	fun(I, N) -> #{nonce => N, player_id => default_id, game_data => I} end, 
	array:from_list(List)
       )
     ).
    

add_transaction_test() ->
    ?assertMatch({_, added_new}, apply_transactions([0])),

    ?assertMatch({_, ignored_duplicate}, apply_transactions([0, 0])),
    ?assertMatch({_, ignored_duplicate}, apply_transactions([2, 0, 3, 0])),

    ?assertMatch({_, updated_old}, apply_transactions(no_repeats([0, 0]))),

    
    ?assertEqual([{0, 0}], get_final_nonces_counters([0, 0])),

    ?assertEqual([{0, 0}, {1, 1}, {2, 2}], get_final_nonces_counters([0, 1, 2])),
    ?assertEqual([0, 1, 2], get_final_nonces([2, 0, 1])),
    ?assertEqual([0, 2], get_final_nonces([2, 0])),
    ?assertEqual([0, 1, 2, 3], get_final_nonces([2, 3, 0, 0, 1, 2])),

    ?assertEqual([{0, 1}, {1, 4}, {2, 5}, {3, 6}], get_final_nonces_counters([2, 0, 3, 1])),
    ?assertEqual([{0, 1}, {1, 4}, {2, 5}, {3, 6}], get_final_nonces_counters([2, 0, 3, 0, 1, 2])),
    ?assertEqual([{0,4},{1,7},{2,10},{3,11}], get_final_nonces_counters(no_repeats([2, 0, 3, 0, 1, 2]))),
    
    {PT1, _} = apply_transactions([ {0, p2}, {2, p1}, {2, p2}, {0, p1}, {1, p2}, {3, p1} ]),

    ?assertEqual([{0, 3}, {2, 4}, {3, 7}], get_nonce_counter_list(p1, PT1)),
    ?assertEqual([{0, 0}, {1, 5}, {2, 6}], get_nonce_counter_list(p2, PT1)),

    ok.
