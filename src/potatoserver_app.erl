%%%-------------------------------------------------------------------
%% @doc potatoserver public API
%% @end
%%%-------------------------------------------------------------------

-module(potatoserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

  potato_logger:init(),
  logger:info("starting potato game 🥔🥔🥔"),
  %% manual startup
  %%{ok, _} = gen_server:start_link(potato_udp, 3541, []),
  %%{ok, PotatoGameSup} = supervisor:start_link(potato_game_sup, []),
  %%{ok, PotatoGameSup}.



  {ok, EverythingSup} = potatoserver_sup:start_link(),
  %% get the potato_game_sup
  %% I guess you could have also registered it and grabbed it globally :\
  {ok, PotatoChildSpec} = supervisor:get_childspec(EverythingSup, potato_game_sup),
  {error,{already_started, PotatoSup}} = supervisor:start_child(EverythingSup, PotatoChildSpec),

  %% TODO create Verifiers
  Verifiers = [],

  potato_game_sup:add_game(PotatoSup, Verifiers),

  %% TODO other stuff 😱

  {ok, EverythingSup}.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
