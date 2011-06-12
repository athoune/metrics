-module(metrics).
-author('mathieu@garambrogne.net').

-export([
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

incr_test() ->
    metrics_server:start_link(),

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