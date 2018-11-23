%% @doc Handles sending messages between local processes. Used for testing pop_verifier.
%% 
%% See main loop function for uses.
%% Each process can register its name with its pid.
%% All messages to the name will be relayed to the pid.
%% All messages are buffered and released in order via send_buffered_messages.

-module(local_message_relay_manager).

-export([
	 start/0,
	 start/1
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

-record(message_handler, 
	{
	 process_map = maps:new(),  % name -> pid map
	 msg_buffer = []	    % message buffer
	}).


%% @doc Starts the message manager process

start(_TrackerFn = {OnStartFn, OnExitFn}) ->
    Pid = spawn_link(fun() -> 
			     OnStartFn(message_handler),
			     message_handler_loop(#message_handler{}, {OnExitFn}) 
		     end),
    Pid.


%% @doc same as start/1 but with no pid tracking

start() -> start({fun() -> ok end, fun() -> ok end}).


%% main loop

message_handler_loop(MsgHandler, Data = {OnExitFn}) ->
    MP = MsgHandler#message_handler.process_map,
    MB = MsgHandler#message_handler.msg_buffer,

    receive
	%% register name
	{add_pid, Key, Pid} ->
	    ?assertEqual(maps:find(Key, MP), error),
	    NewMsgHandler = MsgHandler#message_handler{process_map = maps:put(Key, Pid, MP)};

	%% release buffered message
	{send_buffered_messages, CurrentTime} ->
	    MsgFn = fun({FromAddress, DestAddress, MsgId, MsgData}) -> 
			    Pid = maps:get(DestAddress, MP),
			    Pid ! {net, FromAddress, CurrentTime, MsgId, MsgData}
		    end, 

	    lists:foreach(MsgFn, lists:reverse(MB)),
	    
	    NewMsgHandler = MsgHandler#message_handler{msg_buffer = []};

	%% get currently buffered messages
	{get_all_messages, Pid} ->
	    Pid ! {msg_buffer, MB},
	    NewMsgHandler = MsgHandler;

	%% Send message to name. Gets buffered.
	{net, NetData} ->
	    {FromAddress, DestAddress, _, _} = NetData,

	    ?assertMatch({ok, _}, maps:find(FromAddress, MP)),
	    ?assertMatch({ok, _}, maps:find(DestAddress, MP)),

	    NewMsgHandler = MsgHandler#message_handler{msg_buffer = [NetData | MB]};

	exit ->
	    lists:foreach( fun(Pid) -> Pid ! exit end, maps:values(MP)),
	    NewMsgHandler = exit
    end,
    
    if NewMsgHandler /= exit ->
	    message_handler_loop(NewMsgHandler, Data);
       true ->
	    OnExitFn(),
	    ok
    end.

