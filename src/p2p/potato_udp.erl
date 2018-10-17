-module(potato_udp).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% state maps {game_id} to game instance PID
-spec init(integer()) -> {ok, {gen_udp:socket(), map()}}.
init(Port) ->
  io:format("gen_udp open on port: ~p~n", [Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
  %% add to group?
  {ok, {Socket, maps:new()}}.

%% Never used
handle_call(_E, _From, S) ->
  {noreply, S}.

%% add a {game_id} => Pid to map
handle_cast({add_game,Key,Pid}, S) ->
  {Socket, Map} = S,
  Map2 = maps:put(Key, Pid, Map),
  {noreply, {Socket, Map2}};

%%handle_cast({remove_game,Key}, S) ->

%% TODO probably create verifier type that holds address/port
%% consider adding {validator -> address} mapping as well
%% and then this function will handle formatting the message
handle_cast({send, Address, Port, Msg}, S) ->
  {Socket, _} = S,
  gen_udp:send(Socket, Address, Port, Msg),
  {noreply, S}.

%% TODO log errors or whatever.
handle_info({udp, _Socket, _IP, _InPortNo, Packet}, S) ->
  io:format("got: ~p~n", [Packet]),
  case messages:unpack(Packet) of
    fail ->
      io:format("received garbage packet ~p~n", [Packet]),
      {noreply, S};
    {_GameId, _Data} ->
      %% find {game_id} in map
      %% if found, pass message on
      %% maybe remove from set if message could not be passed on (i.e. game died)?
      {noreply, S}
  end;

handle_info(E, S) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, S}.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(normal, _State) ->
  ok;
terminate(_Reason, _State) ->
  io:format("terminate reason: ~p~n", [_Reason]).
