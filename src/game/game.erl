-module(game).
-behaviour(gen_server).


%%-define(INTERVAL, 60000). % One minute
-define(INTERVAL, 1000). % One minute

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("stdlib/include/assert.hrl").
-include("../potato_records.hrl").


start_link(GameInfo) ->
  gen_server:start_link(?MODULE, GameInfo, []).

-spec init({[#validator{}], integer()}) -> {ok, #game_state{}}.
init({Validators, GameId}) ->
  io:format("starting game with verifiers ~p~n",[Validators]),
  {ok, _TRef} = timer:send_interval(?INTERVAL, timer_trigger),
  GS = #game_state{
    gameid = GameId,
    validators = Validators,
    otherstuff = ok_todo
  },
  {ok, GS}.


handle_info(timer_trigger, State) ->
  logger:info("timer triggered~n",[]),
  %% do whatever you need to do here
  {noreply, State}.

%% TODO
handle_cast(_E, State) ->
  {noreply,State}.

%% TODO
handle_call(_E, _From, S) ->
  {noreply, S}.



code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(normal, _State) ->
  ok;
terminate(_Reason, _State) ->
  io:format("terminate reason: ~p~n", [_Reason]).
