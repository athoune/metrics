-module(metrics_log).
-author('mathieu@garambrogne.net').

-export([
    snapshot/0
    ]).

snapshot() ->
    {_Start, End, Counters, Gauges} = metrics_volatil:snapshot(),
    {ok, Io} = file:open("/tmp/metrics.log", [append, {encoding, utf8}]),
    lists:foreach(fun({K, V}) ->
        io:format(Io, "~w counter ~w ~w~n", [End, K, V])
    end, Counters),
    lists:foreach(fun({K, Median, Min, Max}) ->
        io:format(Io, "~w gauge ~w ~w ~w ~w~n", [End, K, Median, Min, Max])
    end, Gauges),
    file:close(Io),
    ok.

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
