-module(potato_udp).
-behavior(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-type udp_ip() :: string().
-type udp_port() :: integer().
-type udp_address() :: {udp_ip(), udp_port()}.

%% game map maps game id to list of game instances
-type game_map() :: map().

start_link(Port) ->
  gen_server:start_link({local, potato_udp}, potato_udp, Port, []).

%% state maps {game_id} to game PID
-spec init(udp_port()) -> {ok, {gen_udp:socket(), game_map()}}.
init(Port) ->
  %%register(potato_udp, self()),
  io:format("gen_udp open on port: ~p~n", [Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
  %% add to group?
  {ok, {Socket, maps:new()}}.

%% Never used
handle_call(_E, _From, S) ->
  {noreply, S}.

%% add a {game_id, VerifierId} => Pid to map
handle_cast({add_game,{Key, VerId},Pid}, {Socket, Map}) ->
  VerList = maps:get(Key, Map, []),
  Map2 = maps:put(Key, [{VerId, Pid} | VerList], Map),
  {noreply, {Socket, Map2}};

%% remove game_id from map
handle_cast({remove_game,Key}, {Socket, Map}) ->
  Map2 = maps:remove(Key, Map),
  {noreply, {Socket, Map2}};

%% TODO probably create verifier type that holds address/port
%% consider adding {validator -> address} mapping as well
%% and then this function will handle formatting the message
handle_cast({send, {Address, Port}, Msg}, S) ->
  {Socket, _} = S,
  gen_udp:send(Socket, Address, Port, Msg),
  {noreply, S}.

unknown_game_error(Packet, GameId) ->
  %% TODO maybe remove from set if message could not be passed on (i.e. game died)?
  logger:alert("received packet ~p for unknown game id ~p~n",[Packet, GameId]),
  ok.

handle_info({udp, _Socket, _IP, _InPortNo, Packet}, S) ->
  {_, Map} = S,
  logger:debug("got packet: ~p~n", [Packet]),
  case typetato:unpack(Packet) of
    fail ->
      logger:alert("received garbage packet ~p~n", [Packet]),
      {noreply, S};
    {ok, {NetId, Data}} ->
      case NetId of
        {game_id, GameId} ->
          case maps:find(GameId, Map) of
            {ok, VerList} ->
              lists:foreach(fun({_, Pid}) -> Pid ! Data end, VerList);
            error ->
              unknown_game_error(Packet, GameId)
          end;
        {targeted, {GameId, VerifierId}} ->
          case maps:find(GameId, Map) of
            {ok, VerList} ->
              case lists:search(fun({VerId, _}) -> VerId == VerifierId end, VerList) of
                {value, {_, Pid}} ->
                  Pid ! Data;
                false ->
                  %% TODO log verified id as well
                  unknown_game_error(Packet, GameId)
              end;
            error ->
              unknown_game_error(Packet, GameId)
          end
      end,
      {noreply, S}
  end;

handle_info(E, S) ->
  logger:alert("unexpected: ~p~n", [E]),
  {noreply, S}.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(normal, _State) ->
  ok;
terminate(_Reason, _State) ->
  io:format("terminate reason: ~p~n", [_Reason]).
