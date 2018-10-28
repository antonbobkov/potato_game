%%%-------------------------------------------------------------------
%% @doc potatoserver top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(potatoserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  %% TODO don't hardcode port
  supervisor:start_link({local, ?SERVER}, ?MODULE, 4524).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Port) ->
  %% TODO game sup
  {ok, {{rest_for_one, 1, 5},[
    %% start potato_udp, it will register itself under potato_udp (access via whereis(potatoudp))
    #{id => potato_udp,
      start => {potato_udp, start_link, [Port]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [potato_udp]},
    %% start the game supervisor, it will register itself under potato_game_sup (access via whereis(potato_game_sup))
    #{id => potato_game_sup,
      start => {potato_game_sup, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [potato_game_sup]}

    %% TODO start the web3 supervisor, it will create new games inside of the game supervisor
  ]}}.


%%====================================================================
%% Internal functions
%%====================================================================
