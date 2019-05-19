-module(potato_cluster_web_server). 

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
	 start/3,
	 stop/1,
	 basic/3,
	 get_report/3
	]). 

start(Port, LogsDir, DocDir) ->
    Res = inets:start(),
    case Res of
	ok ->
	    ok;
	{error,{already_started,inets}} ->
	    ok;
	_ ->
	    erlang:error(Res)
    end,

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
    %% ok = inets:stop(),

    ok.
    
    
basic(SessionID, Env, Input) -> 
    io:format("basic ~n", []),
    Inp = io_lib:format("~nSession: ~p ~n~n Env: ~p ~n~n Inp: ~p ~n ~n", [SessionID, Env, Input]),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "\n<html>\n<body>\nHello, World!\n</body> <pre>" ++ Inp ++ "</pre>\n</html>" ]).


    

html(Tag, Str) ->
     lists:concat(["<", Tag, ">", Str, "</", Tag, ">\n"]).

pairs_to_html_table(PairList) ->
    lists:concat(
      [
       "<table>\n",
       pairs_to_html_table_r(PairList),
       "</table>\n"
      ]).

pairs_to_html_table_r(PairList) when is_list(PairList)->
    lists:concat(lists:map(fun pairs_to_html_table_r/1, PairList));

pairs_to_html_table_r({Key, Val}) ->
    lists:concat(
      [
       "<tr>\n",
       html("td", Key),
       html("td", Val),
       "</tr>\n"
      ]);

pairs_to_html_table_r({th, Key, Val}) ->
    lists:concat(
      [
       "<tr>\n",
       html("th", Key),
       html("th", Val),
       "</tr>\n"
      ]).

pairs_to_html_table_test() ->
    Mp = [{th, "one", "two"}, {mem1, "hello"}, {"mem2", hi}],
    pairs_to_html_table(Mp),
    ok.

get_time_info() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
    %% StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).    
    StrTime = io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second]),
    
    [
     {"local time", StrTime},
     {"timestamp", erlang:system_time(second)}
    ].

%% round(Number, Precision) ->
%%     P = math:pow(10, Precision),
%%     round(Number * P) / P.

get_ram_info() ->
    {DockerByteMem, _} = string:to_integer(os:cmd("cat /sys/fs/cgroup/memory/memory.usage_in_bytes")),
    DocMem = DockerByteMem / 1000000,
    
    [
     {th, "type", "MB"},
     {"erlang memory", io_lib:format("~.2f", [erlang:memory(total) / 1000000])},
     {"docker memory", io_lib:format("~.2f", [DocMem])}
    ].

get_ext_ram_info() -> os:cmd("free -h").

get_process_info() -> os:cmd("ps aux | grep 'erlang\\\|USER' | grep -v grep").
		      
    

get_info_test() ->
    get_time_info(),
    get_ram_info(),
    get_ext_ram_info(),
    get_process_info(),
    ok.


get_report(SessionID, _Env, _Input) -> 
    io:format("get_report ~n", []),

    StatusInfo = gen_server:call({global, potato_monitor}, get_status_info),

    StatusInfoStr = lists:concat(lists:map(fun pairs_to_html_table/1, StatusInfo)),

    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", 
				lists:concat(
				  [
				   "\n<html>\n", 
				   html("pre", gen_server:call({global, potato_monitor}, get_disk_use)),
				   "\n<hr>\n", 
				   pairs_to_html_table(get_time_info()),
				   "\n<hr>\n", 
				   pairs_to_html_table(get_ram_info()),
				   "\n<hr>\n", 
				   StatusInfoStr,
				   "\n<hr>\n", 
				   html("pre", get_ext_ram_info()),
				   "\n<hr>\n", 
				   html("pre", get_process_info()),
				   "\n<hr>\n", 
				   html("pre", gen_server:call({global, potato_monitor}, get_blocks_tail)),
				   "</html>\n"
				  ])]).
