%% this module contains a supervised socket server thingy
-module(everything_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include_lib("stdlib/include/assert.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% potato_udp (listen/send)

%-top level sup (rest for one)
%  -potato udp (list/send) (access via whereis(potatoudp))
%  -games sup (add/remove)
%    -[game]

init([Port]) ->
  %% TODO game sup
  {ok, {{rest_for_one, 1, 5},[
    #{id => potato_udp,
      start => {potato_udp, start_link, [Port]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [potato_udp]},
    #{id => potato_game_sup,
      start => {potato_game_sup, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor,
      modules => [potato_game_sup]}
  ]}}.
