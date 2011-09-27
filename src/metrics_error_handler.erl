%
% Count errors from the erlang's logger
%

-module(metrics_error_handler).
-author('mathieu@garambrogne.net').

-behaviour (gen_event).

-export([
    init/1,
    terminate/2,
    code_change/3,
    handle_call/2,
    handle_event/2,
    handle_info/2
]).

-export([register_with_logger/0]).

-record(state, {}).

register_with_logger() ->
    error_logger:add_report_handler(?MODULE).

init([]) ->
    {ok, #state{}}.

handle_event({Type, _Gleader, _Infos}, State) ->
    metrics_counter:incr(lists:flatten(io_lib:format("error_handler:~s", [Type]))),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
