%% @doc Process that tracks when other processes finish.
%% 
%% Other processes use add_pid and rem_pid messages to indicate start/stop.
%% When all processes stop, a message is sent to the initiating process.

-module(pid_tracker).

-export([
	 start/0,
	 finish/1
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

%% @doc Starts pid tracking process.
%% 
%% Returns TrackingData = {TrPid, {OnStartFn, OnExitFn}} object to be passed into finish function.
%% TrPid is pid of tracking process.
%% OnStartFn(Data) should be called by a process to register with the tracker.
%% OnExitFn should be called by a registered process to indicate its termination

start() ->
    MyPid = self(),
    TrPid = spawn_link(fun() -> pid_tracker_loop(maps:new(), MyPid) end),

    TrPid ! {add_pid, init_pid, init_entry},
    
    OnStartFn = fun(Data) ->
			TrPid ! {add_pid, self(), Data} 
		end,

    OnExitFn = fun() -> 
		       TrPid ! {rem_pid, self()} 
	       end,

    {TrPid, {OnStartFn, OnExitFn}}.

%% @doc Waits at most 100 ms for all processes to finish.
%% 
%% If there are unfinished processes, their data is returned.


finish(_TrackingData = {TrPid, _}) ->
    TrPid ! {rem_pid, init_pid},

    receive
	pid_track_all_done ->
	    all_done
    after 100 ->
	    TrPid ! {get_map, self()},

	    receive
		{map, Mp} ->
		    {not_done, Mp}
	    after 100 ->
		    erlang:error("no response from tracker process")
	    end

    end.


%% Main process loop

pid_tracker_loop(PidMap, ReportPid) ->
    receive
	{add_pid, Pid, Data} ->
	    ?assertEqual(maps:find(Pid, PidMap), error),
	    pid_tracker_loop(maps:put(Pid, Data, PidMap), ReportPid);

	{rem_pid, Pid} ->
	    NewPidMap = maps:remove(Pid, PidMap),
	    Sz = maps:size(NewPidMap),
	    if Sz /= 0 ->
		    pid_tracker_loop(NewPidMap, ReportPid);
	       true ->
		    ReportPid ! pid_track_all_done,
		    ok
	    end;

	{get_map, Pid} ->
	    Pid ! {map, PidMap},
	    pid_tracker_loop(PidMap, ReportPid);

	_Any ->
	    erlang:error({"unexpected message", _Any})
    end.
