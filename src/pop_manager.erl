%% @doc Manages network/timer messages and unbound blocks for pop_protocol.
%% 
%% Can run on its own in a loop, but is meant to be extended
%% to have more structure, to serve as verifier or player.

-module(pop_manager).

%% -export([
%% 	 new/0,
%% 	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").



%% @doc Makes new container, using pop protocol config data,
%% and external function hooks.
%% 
%% Current hooks are: 
%% - send network message
%% - on_new_block: react to new block added to chain

new(PopConfigDat, FunctionHooks) ->
    ok.

%% @doc Is the block known to us?
%% 
%% It is either in chain, pending as an unbound block,
%% or unknown altogether.
get_block_status(Hash, State) ->
    {in_chain, unbound, unknown}.

on_message(net, send_block_hashes, HashList, State) ->
    State;
on_message(net, send_full_blocks, {Age, BlockList}, State) ->
    State;
on_message(net, request_block_hash_range, Range, State) ->
    State;
on_message(net, request_full_blocks, HashList, State) ->
    State;
on_message(net, send_transactions, TransactionList, State) ->
    State;
on_message(net, subscribe, Address, State) ->
    State.
on_message(timer, Time, State) ->
    State.

%% @doc starts the loop below

start_loop(State) ->
    ok.

loop(State, Tid) ->
    receive 
	{net, MsgId, Data} ->
	    NewState = on_message(net, MsgId, Data, State),
	    loop(NewState, Tid);
	{timer, Time} ->
	    NewState = on_message(timer, Time, State),
	    loop(NewState, Tid);
	exit ->
	    Tid ! exit,
	    done;
	Unexpected ->
	    ?debugVal(Unexpected),
	    erlang:error("unexpected message")
	end.
