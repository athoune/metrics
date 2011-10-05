-module(metrics_csv).
-author('mathieu@garambrogne.net').

-export([
    snapshot/0,
    snapshot/1
    ]).

snapshot() ->
    snapshot("/tmp/metrics").

snapshot(Path) ->
    {_Start, End, Counters, Gauges} = metrics_volatil:raw_snapshot(),
    {ok, IoGauge}   = file:open(Path ++ "_gauge.csv",   [append, {encoding, utf8}]),
    {ok, IoCounter} = file:open(Path ++ "_counter.csv", [append, {encoding, utf8}]),

    lists:foreach(fun({K, V}) ->
        io:format(IoCounter, "~w;~w;~w~n", [timestamp(End), K, V])
    end, Counters),
    file:close(IoCounter),

    lists:foreach(fun({K, V}) ->
        io:format(IoGauge, "~w;~w", [timestamp(End), K]),
        lists:foreach(fun(Values) -> io:format(IoGauge, ";~w", [Values]) end, V),
        io:format(IoGauge,"~n",[])
    end, Gauges),
    file:close(IoGauge),
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
