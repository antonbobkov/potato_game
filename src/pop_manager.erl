%% @doc Manages network messages and unbound blocks for pop_protocol.
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
	 on_message/5
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

new(PopConfigData, FunctionHooks) ->
    #pop_manager{
       pop_chain = pop_protocol:new(PopConfigData),
       function_hooks = FunctionHooks,
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
    

    R1 = pop_protocol:find_block_by_id(Hash, PC),
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

on_message(net, SenderAddress, send_block_hashes, HashList, PopManager) ->
    NetSendFn = PopManager#pop_manager.function_hooks#pop_fun_hooks.net_send,

    FilterFn = fun (Hash) -> get_block_status(Hash, PopManager) == unknown end,

    UnknownHashList = lists:filter(FilterFn, HashList),

    NetSendFn(SenderAddress, request_full_blocks, UnknownHashList),

    PopManager;

on_message(net, SenderAddress, send_full_blocks, {Age, BlockList}, PopManager) ->
    PopManager;

on_message(net, SenderAddress, request_block_hash_range, Range, PopManager) ->
    PopManager;

on_message(net, SenderAddress, request_full_blocks, HashList, PopManager) ->
    PC = PopManager#pop_manager.pop_chain,
    NetSendFn = PopManager#pop_manager.function_hooks#pop_fun_hooks.net_send,

    MapFn = fun (Hash) ->
		    R1 = pop_protocol:find_block_by_id(Hash, PC),
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

on_message(net, _ , send_transactions, TransactionList, PopManager) ->
    PC = PopManager#pop_manager.pop_chain,

    FoldFn = fun (T, PC0) ->
		     {_, PC1} = pop_protocol:add_transaction(T, PC0),
		     PC1
	     end,
    
    NewPC = lists:fold(FoldFn, PC, TransactionList),

    PopManager#pop_manager{pop_chain = NewPC}.

%% @doc starts the loop below

start_loop(PopManager) ->
    ok.

loop(PopManager, Tid) ->
    receive 
	{net, SenderAddress, MsgId, Data} ->
	    NewPopManager = on_message(net, SenderAddress, MsgId, Data, PopManager),
	    loop(NewPopManager, Tid);
	%% {timer, Time} ->
	%%     NewPopManager = on_message(timer, Time, PopManager),
	%%     loop(NewPopManager, Tid);
	exit ->
	    %% Tid ! exit,
	    done;
	Unexpected ->
	    ?debugVal(Unexpected),
	    erlang:error("unexpected message")
	end.
