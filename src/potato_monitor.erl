-module(potato_monitor).
-behavior(gen_server).

-export([
	 init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2
	]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("potato_records.hrl").

make_event(Code, Data, State) ->
    EventFn = State#potato_monitor_data.on_event_fn,
    EventFn(Code, Data),
    ok.


init(InitData) when is_record(InitData, potato_monitor_data) ->
    State = InitData,
    make_event(start, InitData, State),
    {ok, InitData}.

handle_call(get_blocks_tail, From, State) ->
    make_event(get_blocks_tail, From, State),

    JsonConf = State#potato_monitor_data.json_config,
    BlockLogFile = json:get_str(block_log_file, JsonConf),    

    Data = os:cmd("tail -n 42 " ++ BlockLogFile),
    
    {reply, Data, State};

handle_call(get_disk_use, From, State) ->
    make_event(get_disk_use, From, State),

    JsonConf = State#potato_monitor_data.json_config,
    case json:find(directory_to_track, JsonConf) of 
	{ok, TrackDir} ->
	    Data = os:cmd("du -sh " ++ binary_to_list(TrackDir));
	error ->
	    Data = "tracking: no directory_to_track field in config"
    end,
    
    {reply, Data, State};

handle_call(E, From, _S) ->
    erlang:error(unexpected_handle_call, [E, From]).

handle_cast(Data, _State) ->
    erlang:error(unexpected_handle_cast, [Data]).

handle_info(Data, _State) ->
    erlang:error(unexpected_handle_info, [Data]).

code_change(OldVsn, _State, Extra) ->
    erlang:error(unexpected_code_change, [OldVsn, Extra]).

terminate(Reason, State) ->
    make_event(terminate, Reason, State),

    ok.


