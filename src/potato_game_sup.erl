%% this module contains a supervised socket server thingy
-module(potato_game_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include_lib("stdlib/include/assert.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  %% TODO start games
  {ok, {{one_for_one, 1, 5},[]}}.
