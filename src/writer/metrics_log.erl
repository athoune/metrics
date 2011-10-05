-module(metrics_log).
-author('mathieu@garambrogne.net').

-export([
    snapshot/0,
    snapshot/1
    ]).

snapshot() ->
    snapshot("/tmp/metrics.log").

snapshot(Path) ->
    {_Start, End, Counters, Gauges} = metrics_volatil:raw_snapshot(),
    {ok, Io} = file:open(Path, [append, {encoding, utf8}]),
    lists:foreach(fun({K, V}) ->
        io:format(Io, "~w counter ~w ~w~n", [timestamp(End), K, V])
    end, Counters),
    lists:foreach(fun({K, V}) ->
        io:format(Io, "~w gauge ~w", [timestamp(End), K]),
        lists:foreach(fun(Values) -> io:format(Io, " ~w", [Values]) end, V),
        io:format(Io,"~n",[])
    end, Gauges),
    file:close(Io),
    ok.

% Private

timestamp({Mega, Second, _Micro}) ->
    Mega * 1000000 + Second.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

log_test_() ->
    {spawn,
        [{setup,
            fun metrics_test:setup/0,
            fun metrics_test:cleanup/1,

            fun() ->
                ok = metrics:add_writer(metrics_volatil),
                metrics_counter:incr(popo, 42),
                metrics_gauge:append(spam, 2),
                metrics_gauge:append(eggs, 2),
                metrics_gauge:append(spam, 3),
                metrics_gauge:append(eggs, 42),
                metrics_gauge:append(spam, 3),
                snapshot()
            end
        }]}.

-endif.
