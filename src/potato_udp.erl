-module(potato_udp).
-behavior(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-type udp_ip() :: string().
-type udp_port() :: integer().
-type udp_address() :: {udp_ip(), udp_port()}.
-type node_id() :: any().
-type node_address() :: {udp_address(), node_id()}.
-type multi_node_address() :: {udp_address(), [node_id()]}.

%% maps node_id to Pid of the process
-type node_map() :: map().

-type event_fn() :: fun( (atom(), any()) -> any() ).

-record(udp_state, 
	{
	  %% my_registered_name :: any(),
	  socket :: gen_udp:socket(),
	  node_map :: node_map(),
	  event_fn :: event_fn()
	}).


-type state() :: #udp_state{}.

-spec send_event(atom(), any(), state()) -> ok.

send_event(Code, Data, State#udp_state{event_fn = EventFn}) ->
    EventFn(Code, Data),
    ok.


%% start_link(Port) ->
%%     gen_server:start_link({local, potato_udp}, potato_udp, Port, []).

-spec init(udp_port() | {udp_port(), event_fn()}) -> {ok, state()}.

init(Port) -> init({Port, fun(_, _) -> ok end});

init({Port, EventFn}) ->
    %%register(potato_udp, self()),

    {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),

    %% io:format("gen_udp open on port: ~p~n", [Port]),

    State = #udp_state
	{
	  socket = Socket,
	  node_map = maps:new(),
	  event_fn = EventFn
	},

    send_event(start, Port, State),

    {ok, State}.

%% Never used
handle_call(E, From, S) ->
    erlang:error(unexptected_handle_call, [E, From, S]).

-spec handle_cast(Arg, state()) -> {noreply, state()} when
      Arg :: {add_node, node_id(), pid()}
	   | {optimized_send, multi_node_address(), any()}
	   | {send, [node_address()], any()}.

%% add a node_id => Pid to map
handle_cast({add_node, NodeId, Pid}, State) ->

    send_event(add_node, {NodeId, Pid}, State),
    
    NodeMap2 = maps:put(NodeId, Pid, State#udp_state.node_map),
    {noreply, State#udp_state{node_map = NodeMap2}};


%% send a message to a remote host (possibly to multiple nodes)

handle_cast({optimized_send, MultiNodeAddress, Msg}, State = #udp_state{socket = Socket}) ->

    send_event(optimized_send, {MultiNodeAddress, Msg}, State),

    {{IpAddress, Port}, RemoteNodeList} = MultiNodeAddress,

    gen_udp:send(Socket, IpAddress, Port, term_to_binary({RemoteNodeList, Msg})),

    {noreply, State};

handle_cast({send, NodeAddressList, Msg}, State) when is_list(NodeAddressList) ->

    send_event(send, {NodeAddressList, Msg}, State),

    OptimizedNodeAddressList = optimize_routing(NodeAddressList),

    lists:foreach(fun(NodeAddress) -> handle_cast({optimized_send, NodeAddress, Msg}, State) end, OptimizedNodeAddressList),

    {noreply, State}.


%% group messages by network address (so we don't duplicate messages to the same address)

-spec optimize_routing([node_address()]) -> [multi_node_address()].

optimize_routing(NodeAddressList) when is_list(NodeAddressList) ->
    AppendFn = fun({NetAddress, NodeId}, MapIn) ->
			
			maps:put(NetAddress, [NodeId | maps:get(NetAddress, MapIn, [])], MapIn)
		end,

    MessageMap = lists:foldl(AppendFn, maps:new(), NodeAddressList),

    maps:to_list(MessageMap).
    
%% unknown_node_error(Packet, NodeId) ->
%%   %% TODO maybe remove from set if message could not be passed on (i.e. node died)?
%%   logger:alert("received packet ~p for unknown node id ~p~n",[Packet, NodeId]),
%%   ok.

handle_info(_NetData = {udp, _Socket, _IP, _InPortNo, Packet}, S) ->
    {_, NodeMap} = S,

    %% logger:debug("got packet: ~p~n", [Packet]),

    _Data = {NodeList, Msg} = binary_to_term(Packet),

    %% ?debugVal(_Data),

    ForwardFn = fun(NodeId) ->
			 Pid = maps:get(NodeId, NodeMap),
			 Pid ! {net, Msg}
		 end,

    lists:foreach(ForwardFn, NodeList),

    {noreply, S};

handle_info(E, S) ->
    %% logger:alert("unexpected: ~p~n", [E]),
    erlang:error(unexpected_handle_info, [E, S]).
    %% {noreply, S}.


code_change(OldVsn, State, Extra) ->
    erlang:error(unexpected_code_change, [OldVsn, State, Extra]).
    %% {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).
