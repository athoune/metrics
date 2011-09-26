-module(metrics_counter).
-author('mathieu@garambrogne.net').

-export([
    incr/1,
    incr/2,
    get/1,
    reset/1,
    to_list/0,
    to_file/0,
    to_file/1
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec incr(any()) -> 'ok'.
incr(Counter) ->
    gen_event:notify(metrics_event, {incr_counter, Counter, 1}).

-spec incr(any(), integer()) -> 'ok'.
incr(Counter, Inc) ->
    gen_event:notify(metrics_event, {incr_counter, Counter, Inc}).

-spec get(any()) -> integer().
get(Counter) ->
    gen_event:call(metrics_event, {get_counter, Counter}).

-spec reset(any()) -> 'ok'.
reset(Counter) ->
    gen_event:notify(metrics_event, {reset_counter, Counter}).

-spec to_list() -> list(tuple(any(), integer())).
to_list() ->
    gen_event:call(metrics_event, {list_counter}).

-spec to_file() -> 'ok'.
to_file() ->
    {Mega, Second, _} = now(),
    to_file(io_lib:format("/tmp/counter.~w~w.csv", [Mega, Second])).

-spec to_file(string()) -> 'ok'.
to_file(FileName) ->
    {ok, Fd} = file:open(FileName, [write]),
    ok = dump_line(Fd, to_list()),
    file:close(Fd).

dump_line(_Fd, []) ->
    ok;
dump_line(Fd, [{Key, Value}|T]) ->
    file:write(Fd, io_lib:format("~s;~w~n", [Key, Value])),
    dump_line(Fd, T).
