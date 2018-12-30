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
	 msg_buffer = [],	    % message buffer
	 size_hook = none,
	 disconnect_set = sets:new(), % {name, name}
	 last_time_tick = none
	}).


%% @doc Starts the message manager process

start(_TrackerFn = {OnStartFn, OnExitFn}) ->
    Pid = spawn_link(fun() -> 
			     OnStartFn(message_handler),
			     message_handler_loop(#message_handler{}, {OnExitFn}) 
		     end),
    Pid.


%% @doc same as start/1 but with no pid tracking

start() -> start({fun(_) -> ok end, fun() -> ok end}).


release_buffered_messages(MsgHandler) ->
    MP = MsgHandler#message_handler.process_map,
    MB = MsgHandler#message_handler.msg_buffer,
    %% Hook = MsgHandler#message_handler.size_hook,
    DisST = MsgHandler#message_handler.disconnect_set,
    LastTimeTick = MsgHandler#message_handler.last_time_tick,

    ?assertNotEqual(none, LastTimeTick),

    MsgFn = fun({FromAddress, DestAddress, MsgId, MsgData}) -> 
		    IsDisconntected = sets:is_element({FromAddress, DestAddress}, DisST),

		    if IsDisconntected == false ->
			    Pid = maps:get(DestAddress, MP),
			    Pid ! {net, FromAddress, LastTimeTick, MsgId, MsgData};
		       true ->
			    ok
		    end
	    end, 

    lists:foreach(MsgFn, lists:reverse(MB)),

    NewMsgHandler = MsgHandler#message_handler{msg_buffer = [], size_hook = none},
    NewMsgHandler.
    
check_hook(MsgHandler) ->
    MB = MsgHandler#message_handler.msg_buffer,
    Hook = MsgHandler#message_handler.size_hook,
    
    Sz = length(MB),
    case Hook of 
	{L, Pid} when L =< Sz ->
	    Pid ! {msg_buffer, MB},
	    NewMsgHandler = release_buffered_messages(MsgHandler),

	    NewMsgHandler;

	_Else ->
	    MsgHandler
    end.


%% main loop

message_handler_loop(MsgHandler, Data = {OnExitFn}) ->
    MP = MsgHandler#message_handler.process_map,
    MB = MsgHandler#message_handler.msg_buffer,
    Hook = MsgHandler#message_handler.size_hook,
    DisST = MsgHandler#message_handler.disconnect_set,

    receive
	%% register name
	{add_pid, Key, Pid} ->
	    ?assertEqual(maps:find(Key, MP), error),
	    NewMsgHandler = MsgHandler#message_handler{process_map = maps:put(Key, Pid, MP)};

	%% add disconnect
	{add_disconnect, NameFrom, NameTo} ->
	    NewMsgHandler = MsgHandler#message_handler{disconnect_set = sets:add_element({NameFrom, NameTo}, DisST)};

	%% remove all disconnects
	{remove_disconnects} ->
	    NewMsgHandler = MsgHandler#message_handler{disconnect_set = sets:new()};

	%% release buffered messages
	{process_buffered_messages_no_reply, Time} ->
	    NewMsgHandler = release_buffered_messages(MsgHandler#message_handler{last_time_tick = Time});

	{process_buffered_messages_no_reply} ->
	    NewMsgHandler = release_buffered_messages(MsgHandler);

	%% get currently buffered messages
	{process_buffered_messages, Pid} ->
	    Pid ! {msg_buffer, MB},
	    NewMsgHandler = release_buffered_messages(MsgHandler);

	%% Send message to name. Gets buffered.
	{net, NetData} ->
	    {FromAddress, DestAddress, _, _} = NetData,

	    ?assertMatch({ok, _}, maps:find(FromAddress, MP)),
	    ?assertMatch({ok, _}, maps:find(DestAddress, MP)),


	    NewMsgHandler = check_hook(MsgHandler#message_handler{msg_buffer = [NetData | MB] });

	{set_hook, Size, Pid} ->
	    ?assertEqual(none, Hook),
	    NewMsgHandler = check_hook(MsgHandler#message_handler{size_hook = {Size, Pid}});

	%% send timer message
	{timer_tick, CurrentTime} ->

	    MsgFn = fun(Pid) -> 
			    Pid ! {timer_custom, CurrentTime}
		    end,

	    lists:foreach(MsgFn, maps:values(MP)),
	    
	    NewMsgHandler = MsgHandler#message_handler{last_time_tick = CurrentTime};
	exit ->
	    lists:foreach( fun(Pid) -> Pid ! exit end, maps:values(MP)),
	    NewMsgHandler = exit;

	_Any ->
	    NewMsgHandler = erlang:error({"unexpected message", _Any})
    end,
    
    if NewMsgHandler /= exit ->
	    message_handler_loop(NewMsgHandler, Data);
       true ->
	    OnExitFn(),
	    ok
    end.

