-module(metrics).
-author('mathieu@garambrogne.net').

-export([
    snapshot/0,
    dump/1
]).

-spec snapshot() -> tuple(tuple(number(), number(), number()),
    tuple(number(), number(), number()), list(), list()).
snapshot() ->
    gen_server:call(metrics_server, {snapshot}).

dump(Driver) ->
    {Start, End, Counters, Gauges} = snapshot(),
    Driver:write(Start, End, Counters, Gauges).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

incr_test() ->
    application:start(metrics),
    Test = now(),
    metrics_counter:incr(plop, 42),
    ?assertEqual(42, metrics_counter:get(plop)),
    metrics_counter:incr(plop, 1),
    ?assertEqual(43, metrics_counter:get(plop)),
    metrics_counter:reset(plop),
    metrics_counter:incr(plop, 3),
    ?assertEqual(3, metrics_counter:get(plop)),
    ?assert(timer:now_diff(now(),Test) < 500).

-endif.