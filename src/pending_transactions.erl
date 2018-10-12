%% @doc container handling pending transactions.
%% 
%% Transactions can come out of order.
%% Newer transactions rewrite older ones.
%% This structure allows purge of old transactions,
%% to control the size of the buffer, however,
%% it is not implemented here.
%% 
%% Also, as implemented, there is a replay attack where a two user's transactions
%% with same nonces are kept being sent alternatively, forcing that user's
%% transactions in the end. This can be fixed by having the user 
%% send timestamp with his transactions and then only rewrite transactions
%% for which timestamp goes up.
%% (or implement handshake to prevent replays)

-module(pending_transactions).

-export([
	 init/0,
	 add_transaction/2,
	 get_pending_players/1,
	 get_pending_transactions/2
	]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("potato_records.hrl").

%% @doc Initialize the container.

init() ->
    #pending_tx{player_map = maps:new(), counter = 0}.

%% update Pending Transaction container with a new counter and a new transaction map for a player

update_container(PlayerId, TxMp, NewCounter, PendingTx) -> 
    %% erlang too hard

    PM = (PendingTx#pending_tx.player_map)#{PlayerId => TxMp},
    
    PendingTx#pending_tx{player_map = PM, counter = NewCounter}.


%% udpate a counter at a TxMap[Nonce] and return modified map and incremented counter

update_counter_at_nonce(Nonce, {Counter, TxMap}) ->
    {_OldCounter, Data} = maps:get(Nonce, TxMap),
    TxMapNew = maps:put(Nonce, {Counter, Data}, TxMap),
    {Counter + 1, TxMapNew}.



%% update counters in order at all nonces in TxMap starting at NonceStart

update_all_the_counters(NonceStart, Counter, TxMap) -> 

    NonceList = lists:sort(lists:filter(fun (N) -> N >= NonceStart end, maps:keys(TxMap))),
    
    {CounterNew, TxMapNew} = lists:foldl(fun update_counter_at_nonce/2, {Counter, TxMap}, NonceList),
    {CounterNew, TxMapNew}.

%% inserts transaction into the map and updates all counters at nonces >= Nonce

insert_transaction(Nonce, Transaction, Counter, TxMap0) ->
    TxMap1 = maps:put(Nonce, {tmp_counter, Transaction}, TxMap0),
    {CounterNew, TxMap2} = update_all_the_counters(Nonce, Counter, TxMap1),
    {CounterNew, TxMap2}.

%% @doc Adds new transaction to the container. 
%% 
%% Updates counters on later transactions.
%% Returns {Container, message} where message is 
%% ignored_duplicate, updated_old, added_new


add_transaction(Transaction, PendingTx)
  when is_record(PendingTx, pending_tx), 
       is_map(Transaction) ->
    #{
      player_id := PlayerId,
      nonce := Nonce
     } = Transaction,

    #pending_tx{
       player_map = PlMap, 
       counter = Counter
      } = PendingTx,

    %% find player by id; if doesn't exist, add them
    case maps:find(PlayerId, PlMap) of 
	{ok, TxMap} ->
	    ok;

	error ->
	    TxMap = maps:new()
    end,

    %% ?debugVal(Nonce),
    %% ?debugVal(TxMap),
    %% ?debugVal(maps:find(Nonce, TxMap)),
    
    %% find transaction by nonce
    case maps:find(Nonce, TxMap) of 
	{ok, {_, T_old}} ->
	    %% if it exists, see if we need to update it
	    if T_old == Transaction ->
		    Status = ignored_duplicate;

	       T_old /= Transaction ->
		    Status = updated_old
	    end,	    
	    ok;
	error ->
	    Status = added_new
    end,

    if Status == ignored_duplicate ->
	    {PendingTx, Status};
       Status /= ignored_duplicate ->
	    {CounterNew, TxMapNew} = insert_transaction(Nonce, Transaction, Counter, TxMap),

	    {update_container(PlayerId, TxMapNew, CounterNew, PendingTx), Status}
    end.
    

%% @doc Get list of all the players for which there are pending transactions.

get_pending_players(#pending_tx{player_map = PlMap}) ->
    maps:keys(PlMap).

%% @doc Returns pending transactions in the correct order.
%% 
%% For each player P, transactions will start at PlayersStartingNonceMap[P]
%% and will have consecutive nonces (no gaps).
%% 
%% Transactions for players not in PlayersStartingNonceMap will be ignored.

get_pending_transactions(PlayersStartingNonceMap, PendingTx) 
  when is_map(PlayersStartingNonceMap),
       is_record(PendingTx, pending_tx) 
       ->

    PlMap = PendingTx#pending_tx.player_map,

    %% (Key, Value, Acc) applied to PlMap
    FoldFn = fun (PlayerId, TxMap, TransactionListAcc) ->
		     R = maps:find(PlayerId, PlayersStartingNonceMap),
		     case R of
			 {ok, StartingNonce} -> 
			     collect_transactions_from_player(TxMap, StartingNonce, TransactionListAcc);
			 error -> 
			     %% ignore players not in PlayersStartingNonceMap

			     TransactionListAcc
		     end
	     end,

    % unsorted list of tuples {Counter, Transaction}
    TransactionList = maps:fold(FoldFn, [], PlMap),


    TupleCompareFn = fun({Counter1, _}, {Counter2, _}) -> Counter1 =< Counter2 end,
    TupleMapFn = fun({_, Transaction}) -> Transaction end,

    lists:map(TupleMapFn, lists:sort(TupleCompareFn, TransactionList)).

%% extracts transactions from a player, in sequential order, starting at Nonce
collect_transactions_from_player(TxMap, Nonce, TransactionListAcc) ->
    R = maps:find(Nonce, TxMap),
    case R of
	{ok, TxData} -> 
	    %% insert TxData, increment Nonce
	    collect_transactions_from_player(TxMap, Nonce + 1, [TxData | TransactionListAcc]);
	error -> 
	    %% done
	    TransactionListAcc
    end.
