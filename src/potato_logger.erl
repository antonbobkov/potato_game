-module(potato_logger).

-export([init/0]).

init() ->
  %% TODO create filters that prepends module and filenmae stuff
  logger:set_primary_config(
    #{level => info,
     filter_default => log,
     filters => [
        {ignore_sups, {fun logger_filters:progress/2, stop}},
        {log_fileline, {fun pretty_filter/2, nothing}}
      ]}
  ),

  %% TODO set up a handler
  %% Too hard...
  %%logger:set_handler_config(default,
%%    #{level = all,
  %%  module = logger_std_h,
%%    formatter = {logger_formatter, }})
  ok.

-spec pretty_filter(filter:log_event(), nothing) -> logger:filter_return().
pretty_filter(LogEvent, _) ->
  %% TODO formatting HALP
  LogEvent.
  %%#{level := _Level,
    %%msg := Msg,
%%    meta := Meta} = LogEvent,

  %%Msg2 = io_lib:format("~p ~p", Meta, Msg),
  %%LogEvent#{msg := Msg2}.

%%#{pid = _Pid,
%%  gl = _Gl,
%%  time = _Time,
%%  mfa = _MFA,
%%  file = File,
%%  line = Line,
%%  domain = _Domain,
%%  report_cb = _ReportCB} = Meta,

%%Msg2 = io_lib:format("~p ~p ~p", File, Line, Msg),
%%LogEvent#{msg := Msg2}.
