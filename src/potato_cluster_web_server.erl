-module(potato_cluster_web_server). 

-export([
	 start/3,
	 stop/1,
	 service/3
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
    
    

service(SessionID, Env, Input) -> 
    io:format("service ~n", []),
    Inp = io_lib:format("~nSession: ~p ~n~n Env: ~p ~n~n Inp: ~p ~n ~n", [SessionID, Env, Input]),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "\n<html>\n<body>\nHello, World!\n</body> <pre>" ++ Inp ++ "</pre>\n</html>" ]).
