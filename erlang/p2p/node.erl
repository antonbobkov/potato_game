-module(node).

-export([test/1]).

-include("../blocktree/blocktree.hrl").
-include_lib("stdlib/include/assert.hrl").

test(Tp) ->
    io:fwrite("hello, world\n"),
    if Tp == 1 ->
            init();
       Tp == 2 ->
            client()
    end.

client() ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678, 
                                 [binary, {active, true}]),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).

init() ->
    %% start listening server
    {ok, LSock} = gen_tcp:listen(5678, [binary, {active, true}]),
    %% TODO crash if not ok
    tcp_listener(LSock).

tcp_listener(LSock) ->
    io:fwrite("listening"),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:fwrite("accetp"),
    %% TODO crash if not ok
    spawn(?MODULE, handle, [Sock]),
    %% keep listening
    tcp_listener(LSock).

handle(Socket) ->
    io:fwrite("handling socket"),
    %% initiate handshake
    %% wait for handshake
    %% go to receive handle
    inet:setopts(Socket, [{active, true}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            %%gen_tcp:send(Socket, Msg),
            io:fwrite(Msg),
            handle(Socket)
    end.

