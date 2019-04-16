-module(potato_cluster_web_server). 

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
	 start/3,
	 stop/1,
	 basic/3,
	 get_system_info/3
	]). 

start(Port, LogsDir, DocDir) ->
    ok = inets:start(),
    {ok, Reference} = 
	inets:start(httpd, [ 
			     {modules,
			      [mod_alias, 
			       mod_auth, 
			       mod_esi, 
			       mod_actions, 
			       mod_cgi, 
			       mod_dir,
			       mod_get, 
			       mod_head, 
			       mod_log, 
			       mod_disk_log 
			      ]
			     }, 

			     {port, Port}, 
			     {server_name, "potato_web_server_name"}, 
			     {server_root, "."}, 
			     {document_root, DocDir}, 
			     {erl_script_alias, {"/erl", [potato_cluster_web_server]}}, 
			     {erl_script_nocache, true},
			     {error_log, LogsDir ++ "/error.log"}, 
			     {security_log, LogsDir ++ "/security.log"}, 
			     {transfer_log, LogsDir ++ "/transfer.log"}, 

			     {mime_types,[ 
					   {"html","text/html"}, 
					   {"css","text/css"}, 
					   {"js","application/x-javascript"} 
					 ]
			     } 
			   ]
		   ), 

    %% io:format("started ~n", []),

    Reference. 

stop(Reference) ->
    ok = inets:stop(httpd, Reference), 
    ok = inets:stop(),

    ok.
    
    
basic(SessionID, Env, Input) -> 
    io:format("basic ~n", []),
    Inp = io_lib:format("~nSession: ~p ~n~n Env: ~p ~n~n Inp: ~p ~n ~n", [SessionID, Env, Input]),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "\n<html>\n<body>\nHello, World!\n</body> <pre>" ++ Inp ++ "</pre>\n</html>" ]).


get_system_info() ->
    Time1 = erlang:localtime(),
    Time2 = erlang:system_time(second),

    % Erlang memory usage
    ErlMem = erlang:memory(),
    ErlMemTotal = erlang:memory(total) / 1000000,

    % extenal os memory usage
    SysMem = os:cmd("free -mt"),

    % docker memory usage
    {DockerByteMem, _} = string:to_integer(os:cmd("cat /sys/fs/cgroup/memory/memory.usage_in_bytes")),
    DocMem = DockerByteMem / 1000000,

    [Time1, Time2, line, ErlMem, ErlMemTotal, line, SysMem, line, DocMem].

list_to_html_pre(List) ->

    MapFn = fun 
		(line) ->
		    io_lib:format("~n", []);
		(Data) ->
		    io_lib:format("~p~n", [Data])
	    end,

    StrList = lists:map(MapFn, List),

    
    Final = lists:concat(["<pre>"] ++ StrList ++ ["</pre>\n"]),
    
    Final.
	


get_system_info_test() -> list_to_html_pre(get_system_info()).

get_system_info(SessionID, Env, Input) -> 
    io:format("get_system_info ~n", []),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "\n<html>\n" ++ list_to_html_pre(get_system_info()) ++ "</html>\n" ]).

