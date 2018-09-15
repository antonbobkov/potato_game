-module(web3).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
% TODO actually hook up to web3 XD

%% how many coop chain blocks to wait before consider verified
% -define(K, 10).

%%
init(_) ->
  %?debugMsg("starting web3 server..."),
  Pid = self(),
  spawn_link(fun() -> height_poll("", Pid) end),
  {ok, -1}.

%% return current block number
handle_call(_, _From, BlockNum) ->
  %?debugMsg("handlecall"),
  {reply, max(-1,BlockNum - 10), BlockNum}.


handle_cast(Cmd, BlockNum) when Cmd == stop ->
  {stop, ok, BlockNum};
handle_cast(NewBlockNum, _) ->
  %?debugMsg("handlecast"),
  {noreply, NewBlockNum}.



height_poll(Provider, _ReplyPid)->
  %TODO actually poll
  timer:sleep(100),
  %?debugFmt("polling... ~p~n", [_ReplyPid]),
  gen_server:cast(_ReplyPid, 123),
  height_poll(providerurl, _ReplyPid).
