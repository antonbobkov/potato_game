%% @doc potato_game_sup supervisor manages individual game instances
%%
%% Each game instance is added using add_game(Verifiers)
%%
%% This has the code that responds to block requests.
%% Current idea is that only verifiers should do that,
%% though it is not inconcievable that players can also
%% provide that information to each other.
-module(potato_game_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, add_game/2]).

-include_lib("stdlib/include/assert.hrl").

start_link() ->
  %% this will register with name potato_game_sup I think?
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("starting potato_game_sup~n"),
  {ok, {{simple_one_for_one, 1, 5},[
  #{id => game,
    start => {game, start_link, []},
    restart => transient, %% transient means games that terminate normally will not be restarted
    shutdown => brutal_kill,
    type => worker,
    modules => [game]}
  ]}}.

-spec add_game(supervisor:sup_ref(), list()) -> pid() | error.
add_game(SupRef, Verifiers) ->
  case supervisor:start_child(SupRef,[Verifiers]) of
    {ok, Child} -> Child;
    {ok, Child, _} -> Child;
    {error, _} -> error
  end,
error.
