%% this module contains a supervised socket server thingy
-module(supersocket).
-behaviour(supervisor).

-export([start_socket_server/0]).
-export([start_socket/0]).
-export([init/1]).

-include_lib("stdlib/include/assert.hrl").

start_socket_server() ->
  io:fwrite("starting socket server on port 5678\n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
  Port = 5678,
  %% what is {packet, line} idk?
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}]),
  spawn_link(fun empty_listeners/0),
  {ok, {{simple_one_for_one, 60, 3600},[
    {socket,
    {supersocket_tcplistener, start_link, [ListenSocket]}, % pass the socket!
    temporary, 1000, worker, [sockserv_serv]}
  ]}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

%% Start with 20 listeners so that many multiple connections can join at once
%% without waiting on previous one to finish connecting.
empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.
