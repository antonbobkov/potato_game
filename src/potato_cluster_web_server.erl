-module(potato_cluster_web_server). 

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
	 start/3,
	 stop/1,
	 basic/3,
	 get_system_info/3,
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

get_system_info(SessionID, _Env, _Input) -> 
    io:format("get_system_info ~n", []),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "\n<html>\n" ++ list_to_html_pre(get_system_info()) ++ "</html>\n" ]).

%% str_pre(Str) ->
%%     lists:concat(["<pre>", Str, "</pre>\n"]).

html(Tag, Str) ->
     lists:concat(["<", Tag, ">", Str, "</", Tag, ">\n"]).

pairs_to_html_table(PairList, H1, H2) ->
    lists:concat(
      [
       "<table>",
       html("th", H1), 
       html("th", H2),
       pairs_to_html_table(PairList),
       "</table>\n"
      ]).

pairs_to_html_table(PairList) when is_list(PairList)->
    lists:concat(lists:map(fun pairs_to_html_table/1, PairList));

pairs_to_html_table({Key, Val}) ->
    lists:concat(
      [
       "<tr>",
       html("td", Key),
       html("td", Val),
       "</tr>\n"
      ]).

pairs_to_html_table_test() ->
    Mp = [{mem1, "hello"}, {mem2, hi}],
    pairs_to_html_table(Mp, "h1", "h2"),
    ok.

get_report(SessionID, _Env, _Input) -> 
    io:format("get_report ~n", []),
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", 
				lists:concat(
				  [
				   "\n<html>\n", 
				   html("pre", gen_server:call({global, potato_monitor}, get_disk_use)),
				   html("pre", gen_server:call({global, potato_monitor}, get_blocks_tail)),
				   pairs_to_html_table([{mem1, "hello"}, {mem2, hi}], "h1", "h2"),
				   "</html>\n"
				  ])]).
