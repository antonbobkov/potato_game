-module(hello_web). 
-export([start/0,service/3]). 

start() ->
    ok = inets:start(),
    {ok, _} = inets:start(httpd, [ 
				   {modules, [ 
						   mod_alias, 
					       mod_auth, 
					       mod_esi, 
					       mod_actions, 
					       mod_cgi, 
					       mod_dir,
					       mod_get, 
					       mod_head, 
					       mod_log, 
					       mod_disk_log 
					     ]}, 

				   {port,8081}, 
				   {server_name,"helloworld"}, 
				   {server_root,"."}, 
				   {document_root,"."}, 
				   {erl_script_alias, {"/erl", [hello_web]}}, 
				   {error_log, "error.log"}, 
				   {security_log, "security.log"}, 
				   {transfer_log, "transfer.log"}, 

				   {mime_types,[ 
						 {"html","text/html"}, {"css","text/css"}, {"js","application/x-javascript"} ]} 
				 ]), 

    io:format("started ~n", []),
    ok. 
         
service(SessionID, _Env, _Input) -> 
    io:format("service ~n", []),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "\n<html>\n<body>\nHello, World!\n</body>\n</html>" ]).
