-module(potato_udp).
-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start_link() ->
    gen_server:start_link({local, potato_udp}, potato_udp, [], []).

%% state maps {game_id} to game PID
-spec init(integer()) -> {ok, {gen_udp:socket(), map()}}.
init(Port) ->
  register(potatoudp, self()),
  io:format("gen_udp open on port: ~p~n", [Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
  %% add to group?
  {ok, {Socket, maps:new()}}.

%% Never used
handle_call(_E, _From, S) ->
  {noreply, S}.

%% add a {game_id} => Pid to map
handle_cast({add_game,Key,Pid}, {Socket, Map}) ->
  Map2 = maps:put(Key, Pid, Map),
  {noreply, {Socket, Map2}};

%% remove game_id from map
handle_cast({remove_game,Key}, {Socket, Map}) ->
  Map2 = maps:put(Key, Map),
  {noreply, {Socket, Map2}};

%% TODO probably create verifier type that holds address/port
%% consider adding {validator -> address} mapping as well
%% and then this function will handle formatting the message
handle_cast({send, {Address, Port}, Msg}, S) ->
  {Socket, _} = S,
  gen_udp:send(Socket, Address, Port, Msg),
  {noreply, S}.

%% TODO log errors or whatever.
handle_info({udp, _Socket, _IP, _InPortNo, Packet}, S) ->
  {_, Map} = S,
  io:format("got: ~p~n", [Packet]),
  case messages:unpack(Packet) of
    fail ->
      io:format("received garbage packet ~p~n", [Packet]),
      {noreply, S};
    {GameId, Data} ->
      case maps:find(GameId, Map) of
        %% maybe remove from set if message could not be passed on (i.e. game died)?
        {ok, Pid} -> Pid ! Data;
        error -> ok
      end,
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
