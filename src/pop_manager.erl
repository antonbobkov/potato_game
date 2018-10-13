%% @doc Manages network messages and unbound blocks for pop_chain.
%% 
%% Can run on its own in a loop, but is meant to be extended
%% to have more structure, to serve as verifier or player.
%% Those will be implemented in pop_verifier.erl and pop_player.erl
%% 
%% This has the code that responds to block requests.
%% Current idea is that only verifiers should do that,
%% though it is not inconcievable that players can also
%% provide that information to each other.

-module(pop_manager).

-export([
	 new/2,
	 on_net_message/4
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

%% @doc Makes new container, using pop protocol config data,
%% and external function hooks.
%% 
%% Current hooks are: 
%% - send network message
%% - on_new_block: react to new block added to chain

new(PopConfigData, PopManagerConfig) ->
    #pop_manager{
       pop_chain = pop_chain:new(PopConfigData),
       config = PopManagerConfig,
       unbound_blocks = maps:new()
      }.
    

%% Is the block known to us?
%% 
%% It is either in chain, pending as an unbound block,
%% or unknown altogether.
%% Returns in_chain, unbound, or unknown
get_block_status(Hash, PopManager) ->
    #pop_manager{
       pop_chain = PC,
       unbound_blocks = UB
      } = PopManager,
    

    R1 = pop_chain:find_block_by_id(Hash, PC),
    case R1 of
	{ok, _} ->
	    in_chain;
	error ->
	    R2 = maps:find(Hash, UB),
	    case R2 of
		{ok, _} ->
		    unbound;
		error ->
		    unknown
	    end
    end.

%% get previous block
get_prev(Block, PC) ->
    {ok, Prev} = pop_chain:find_block_by_id(maps:get(prev_id, Block), PC),
    Prev.

%% get n'th previous block, stop at genisys block
get_nth_prev_block(N, Block, PC) ->
    Ht = maps:get(height, Block),
    if (Ht == 0) or (N == 0) ->
	    Block;
       (Ht > 0) and (N > 0) ->
	    get_nth_prev_block(N-1, get_prev(Block, PC), PC)
    end.

%% given unknown block request, and longest known block from another party
%% make a best guess about which blocks are unknown to the other party
%% and return a array of hashes of those blocks

compute_block_hash_range(UnknownBlockHash, KnownBlockHash, PopManager) ->
    PC = PopManager#pop_manager.pop_chain,
    UnknownBlock = pop_chain:find_block_by_id(UnknownBlockHash, PC),
    KnownBlock = pop_chain:find_block_by_id(KnownBlockHash, PC),

    compute_block_hash_range(UnknownBlock, KnownBlock, PC, []).

compute_block_hash_range(UnknownBlock, KnownBlock, PC, AccList) ->
    UnknownId = maps:get(this_id, UnknownBlock),
    KnownId = maps:get(this_id, KnownBlock),

    UnknownHeight = maps:get(height, UnknownBlock),
    KnownHeight = maps:get(height, KnownBlock),

    if UnknownId == KnownId ->
	    AccList;  % done, found common parent

       UnknownId /= KnownId ->
	    %% decrement higher block, or both if equal height
	    %% each time Unknown is decremented, add its hash to the list

	    if UnknownHeight >= KnownHeight -> 	
		    UnknownBlockNew = get_prev(UnknownBlock, PC), 
	            KnownBlockNew = KnownBlock,
	            AccListNew = [UnknownId | AccList];

	       UnknownHeight =< KnownHeight ->
		    UnknownBlockNew = UnknownBlock, 
	            KnownBlockNew = get_prev(KnownBlock, PC),
	            AccListNew = AccList;

	       UnknownHeight == KnownHeight ->
		    UnknownBlockNew = get_prev(UnknownBlock, PC), 
	            KnownBlockNew = get_prev(KnownBlock, PC),
	            AccListNew = [UnknownId | AccList]
	    end,

	    compute_block_hash_range(UnknownBlockNew, KnownBlockNew, PC, AccListNew)
    end.
    
	    
	    
    

on_net_message(SenderAddress, send_block_hashes, HashList, PopManager) ->
    NetSendFn = PopManager#pop_manager.config#pop_manager_config.net_send,

    FilterFn = fun (Hash) -> get_block_status(Hash, PopManager) == unknown end,

    UnknownHashList = lists:filter(FilterFn, HashList),

    NetSendFn(SenderAddress, request_full_blocks, UnknownHashList),

    PopManager;

on_net_message(SenderAddress, send_full_blocks, {Age, BlockList}, PopManager) ->
    PopManager;

on_net_message(SenderAddress, request_block_hash_range, {KnownHash1, KnownHash2, UnknownHash}, PopManager) ->
    NetSendFn = PopManager#pop_manager.config#pop_manager_config.net_send,

    St1 = get_block_status(KnownHash1, PopManager),
    St2 = get_block_status(KnownHash2, PopManager),
    
    if 
	St1 == in_chain ->
	    KnownHash = KnownHash1;
	St2 == in_chain ->
	    KnownHash = KnownHash2;
	true ->
	    KnownHash = maps:get(this_id, pop_chain:get_genisys_block(PopManager#pop_manager.pop_chain))
    end,

    RequestHashList = compute_block_hash_range(UnknownHash, KnownHash, PopManager),

    NetSendFn(SenderAddress, send_block_hashes, RequestHashList),

    PopManager;

on_net_message(SenderAddress, request_full_blocks, HashList, PopManager) ->
    PC = PopManager#pop_manager.pop_chain,
    NetSendFn = PopManager#pop_manager.config#pop_manager_config.net_send,

    MapFn = fun (Hash) ->
		    R1 = pop_chain:find_block_by_id(Hash, PC),
		    case R1 of
			{ok, Block} ->
			    NetSendFn(SenderAddress, send_full_blocks, {old, [Block]});
			error ->
			    debug_only:error("unexpected request_full_blocks, unknown hash")
		    end,
		    Hash
	    end,

    lists:map(MapFn, HashList),

    PopManager;

on_net_message(_ , send_transactions, TransactionList, PopManager) ->
    PC = PopManager#pop_manager.pop_chain,

    FoldFn = fun (T, PC0) ->
		     {_, PC1} = pop_chain:add_transaction(T, PC0),
		     PC1
	     end,
    
    NewPC = lists:fold(FoldFn, PC, TransactionList),

    PopManager#pop_manager{pop_chain = NewPC}.

%% @doc Loop that handles messages.

loop(PopManager) ->
    receive 
	{net, SenderAddress, MsgId, Data} ->
	    NewPopManager = on_net_message(SenderAddress, MsgId, Data, PopManager),
	    loop(NewPopManager);
	exit ->
	    done;
	Unexpected ->
	    ?debugVal(Unexpected),
	    erlang:error("unexpected message")
    end.
