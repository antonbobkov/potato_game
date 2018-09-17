-module(supersocket_tcplistener).
-behaviour(gen_server).

-record(state, {name, % connection id nonsense idk
next, % next step, used when initializing
socket}). % the current socket

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


-define(TIME, 800).
-define(EXP, 50).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
  %% Because accepting a connection is a blocking function call,
  %% we can not do it in here. Forward to the server loop!
  gen_server:cast(self(), accept),
  {ok, #state{socket=Socket}}.

%% Never used
handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
  %% listen for connection
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),

  %% once we get a connection, start a new listener to replace us
  supersocket:start_socket(),

  %% server initiates handshake stuff
  ok = gen_tcp:send(AcceptSocket, io_lib:format("Welcome to my 🥔🥔🥔 server~n", [])),

  %% once we send out handshake, start actively listening for a response
  ok = inet:setopts(AcceptSocket, [{active, once}]),

  %% go to next step
  {noreply, S#state{socket=AcceptSocket, next=handshake}}.


%% Let's get rid of the white space and ignore whatever's after.
%% makes it simpler to deal with telnet.
line(Str) ->
  hd(string:tokens(Str, "\r\n ")).

%% use handle info to pick up on active tcp messages
handle_info({tcp, _Socket, Str}, S = #state{next=handshake}) ->
  Name = line(Str),
  %% TODO figure out who connected and send message to something else to start a worker to handle it
  %% make sure to pass on ownership of socket to the next owner
  %% make sure the next owner sts socket to {active, once}
  %%  make sure there's no race condition where tcp packets get lost in ownership transfer (would be awful if there was...)


  {noreply, S#state{name=Name, next=done}};

%% TODO log errors or whatever.
handle_info({tcp_closed, _Socket}, S) ->
  {stop, normal, S};
handle_info({tcp_error, _Socket, _}, S) ->
  {stop, normal, S};
handle_info(E, S) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, S}.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(normal, _State) ->
  ok;
terminate(_Reason, _State) ->
  io:format("terminate reason: ~p~n", [_Reason]).
