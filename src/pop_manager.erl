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
	 on_net_message/5,
	 %% fn_convert_to_multi_address
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

%% doc Converts single address send fun to address list send fun.
%% Send message to each address in list one by one.

%% fn_convert_to_multi_address(NetSendFn) ->
%%     fun(AddressList, MsgId, Data) ->
%% 	    lists:foreach(fun(Address) -> NetSendFn(Address, MsgId, Data) end, AddressList)
%%     end.

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
    {ok, Prev} = pop_chain:find_block_by_id(maps:get(previous_id, Block), PC),
    Prev.

%% get n'th previous block, stop at genisys block
get_nth_prev_block(N, Block, PC) ->
    Ht = maps:get(height, Block),
    if (Ht == 0) or (N == 0) ->
	    Block;
       (Ht > 0) and (N > 0) ->
	    get_nth_prev_block(N-1, get_prev(Block, PC), PC)
    end.

setup_range_request(LastUnknownBlockHash, PopManager) ->
    PC = PopManager#pop_manager.pop_chain,
    N = PopManager#pop_manager.config#pop_manager_config.request_range_backup,

    KnownBlock1 = pop_chain:get_head_block(PC),
    KnownBlock2 = get_nth_prev_block(N, KnownBlock1, PC),
    
    {maps:get(this_id, KnownBlock1), maps:get(this_id, KnownBlock2), LastUnknownBlockHash}.

%% given unknown block request, and longest known block from another party
%% make a best guess about which blocks are unknown to the other party
%% and return a array of hashes of those blocks

compute_block_hash_range(UnknownBlockHash, KnownBlockHash, PopManager) ->
    PC = PopManager#pop_manager.pop_chain,
    {ok, UnknownBlock} = pop_chain:find_block_by_id(UnknownBlockHash, PC),
    {ok, KnownBlock} = pop_chain:find_block_by_id(KnownBlockHash, PC),

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

	    if UnknownHeight > KnownHeight -> 	
		    UnknownBlockNew = get_prev(UnknownBlock, PC), 
	            KnownBlockNew = KnownBlock,
	            AccListNew = [UnknownId | AccList];

	       UnknownHeight < KnownHeight ->
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
    
%% Adds a new block into pop_chain.
%% Then it checks unbound blocks, and adds all that become bound.
%% Function hook on_new_block is triggered on each addition.

add_block_in_order(Block, CurrentTime, PopManager0) ->
    PC0 = PopManager0#pop_manager.pop_chain,
    UnboundMap0 = PopManager0#pop_manager.unbound_blocks,

    OnNewBlockFn = PopManager0#pop_manager.config#pop_manager_config.on_new_block,

    PC1 = pop_chain:add_block_in_order(Block, CurrentTime, PC0),

    OnNewBlockFn(Block),

    %% process unbound blocks, recursively add new ones as needed

    IsUnboundPred = 
	fun({_Id, B}) ->
		R = pop_chain:find_block_by_id(maps:get(previous_id, B), PC1),
		case R of
		    {ok, _} ->
			false;
		    error ->
			true
		end
	end,

    {UnboundList, BoundList} = lists:partition(IsUnboundPred, maps:to_list(UnboundMap0)),
    
    UnboundMap1 = maps:from_list(UnboundList),

    PopManager1 = PopManager0#pop_manager{
		    pop_chain = PC1,
		    unbound_blocks = UnboundMap1
		   },

    FoldFn = fun({_Id, NewBlock}, PM) -> add_block_in_order(NewBlock, CurrentTime, PM) end,

    PopManager2 = lists:foldl(FoldFn, PopManager1, BoundList),
    
    PopManager2.
			  
    
	    
%% Processes a new block, either ignoring a duplicate, 
%% storing it as unbound for later inserting, or inserting it.

add_block_any_order(Block, CurrentTime, PopManager) ->

    PC = PopManager#pop_manager.pop_chain,
    UnboundMap = PopManager#pop_manager.unbound_blocks,

    ThisId = maps:get(this_id, Block),
    PrevId = maps:get(previous_id, Block),

    R1 = pop_chain:find_block_by_id(ThisId, PC),
    case R1 of
	{ok, _} ->
	    Status = ignored_duplicate;
	error ->
	    R2 = pop_chain:find_block_by_id(PrevId, PC),
	    case R2 of
		{ok, _} ->
		    Status = added_new;
		error ->
		    Status = unbound
	    end	    
    end,

    if Status == added_new ->
	    {Status, add_block_in_order(Block, CurrentTime, PopManager)};

       Status == unbound ->
	    UnboundMapNew = maps:put(ThisId, Block, UnboundMap),
	    {Status, PopManager#pop_manager{unbound_blocks = UnboundMapNew}};

       Status == ignored_duplicate ->
	    {Status, PopManager}
    end.


%% @doc Receive hashes, request unknown blocks.

on_net_message(SenderAddress, _, send_block_hashes, HashList, PopManager) ->
    NetSendFn = PopManager#pop_manager.config#pop_manager_config.net_send,

    FilterFn = fun (Hash) -> get_block_status(Hash, PopManager) == unknown end,

    UnknownHashList = lists:filter(FilterFn, HashList),

    NetSendFn(SenderAddress, request_full_blocks, UnknownHashList),

    PopManager;

%% @doc Receive blocks, add them to the structure.

on_net_message(SenderAddress, CurrentTime, send_full_blocks, {Age, BlockList}, PopManager) ->
    NetSendFn = PopManager#pop_manager.config#pop_manager_config.net_send,

    FoldFn = 
	fun(Block, PM) ->
		PrevHash = maps:get(previous_id, Block),

		PrevBlockStatus = get_block_status(PrevHash, PM),

		if (Age == new) and (PrevBlockStatus /= in_chain) ->
			NetSendFn(SenderAddress, request_block_hash_range, setup_range_request(PrevHash, PM));
		   true -> ok
		end,
		
		{_Status, NewPM} = add_block_any_order(Block, CurrentTime, PM),
		NewPM
	end,

    NewPopManager = lists:foldl(FoldFn, PopManager, BlockList),
    NewPopManager;

%% @doc Respond to hash range request by computing and sending the range.

on_net_message(SenderAddress, _, request_block_hash_range, {KnownHash1, KnownHash2, UnknownHash}, PopManager) ->
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

%% @doc Respond to block request by sending all the known requested blocks.

on_net_message(SenderAddress, _, request_full_blocks, HashList, PopManager) ->
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

%% @doc Process the received transactions.

on_net_message(_, _, send_transactions, TransactionList, PopManager) ->
    PC = PopManager#pop_manager.pop_chain,

    FoldFn = fun (T, PC0) ->
		     {_, PC1} = pop_chain:add_transaction(T, PC0),
		     PC1
	     end,
    
    NewPC = lists:foldl(FoldFn, PC, TransactionList),

    PopManager#pop_manager{pop_chain = NewPC};

on_net_message(SenderAddress, CurrentTime, MsgId, MsgData, PopManager) ->
    erlang:error(unknown_net_message, [SenderAddress, CurrentTime, MsgId, MsgData, PopManager]).
