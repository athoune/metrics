-module(metrics).
-author('mathieu@garambrogne.net').

-export([
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

incr_test() ->
    metrics_server:start_link(),

    metrics_timer:init(test),
    metrics_counter:incr(plop, 42),
    ?assertEqual(42, metrics_counter:get(plop)),
    metrics_counter:incr(plop, 1),
    ?assertEqual(43, metrics_counter:get(plop)),
    metrics_counter:reset(plop),
    metrics_counter:incr(plop, 3),
    ?assertEqual(3, metrics_counter:get(plop)),
    ?assert(metrics_timer:get(test) < 500),
    ?assert(metrics_timer:exists(test)),
    metrics_timer:init(test),
    metrics_timer:init(walrus),
    ?assert(metrics_timer:exists(walrus)),
    metrics_timer:get_and_reset(walrus).

-endif.