-module(potato_socket).

-export([start/0, start_server/0, start_client/0, handle_server_side/1, handle_client_side/1]).

-include_lib("stdlib/include/assert.hrl").

start() ->
    io:fwrite("hello, world\n"),
    spawn(?MODULE, start_server, []),
    timer:sleep(100), % need to sleep otherwise there is a race cond between server/client
    spawn(?MODULE, start_client, []).


start_client() ->
    io:fwrite("starting client\n"),
    {ok, Sock} = gen_tcp:connect("localhost", 5678, [binary, {active, true}]),
    gen_tcp:send(Sock, "Some Data"),
    handle_client_side(Sock).

handle_client_side(Socket) ->
    %io:fwrite("handling socket client\n"),
    receive
        {tcp, Socket, Msg} ->
            %%gen_tcp:send(Socket, Msg),
            io:fwrite("client got message ~p\n", [Msg]),
            gen_tcp:close(Socket);
        X ->
            io:fwrite("unkown match ~p\n", [X])
    end.

start_server() ->
    io:fwrite("starting server \n"),
    {ok, LSock} = gen_tcp:listen(5678, [binary, {active, true}]),
    tcp_listener(LSock).

tcp_listener(LSock) ->
    %io:fwrite("listening\n"),
    % for some reason this will return {error, closed} when handle_server_side returns...
    {ok, Sock} = gen_tcp:accept(LSock),
    io:fwrite("accept\n"),
    spawn(fun() -> tcp_listener(LSock) end),
    handle_server_side(Sock).

handle_server_side(Socket) ->
    %io:fwrite("handling socket server \n"),
    inet:setopts(Socket, [{active, true}]),
    receive
        {tcp, Socket, Msg} ->
            io:fwrite("server got msg ~p\n", [Msg]),
            io:fwrite("echoing ~p\n", [Msg]),
            gen_tcp:send(Socket, Msg),
            handle_server_side(Socket);
        X ->
            io:fwrite("unkown match ~p\n", [X]),
            gen_tcp:close(Socket)
    end.
