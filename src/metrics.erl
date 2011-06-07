-module(metrics).
-author('mathieu@garambrogne.net').

-export([
    incr_counter/2,
    get_counter/1,
    reset_counter/1,
    start_timer/1,
    get_timer/1,
    append_gauge/2,
    erase_gauge/1,
    get_gauge/1,
    min_max/1,
    mean/1
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

incr_counter(Key, Inc) ->
    gen_server:cast(metrics_server, {incr_counter, Key, Inc}).

get_counter(Key) ->
    gen_server:call(metrics_server, {get_counter, Key}).

reset_counter(Key) ->
    gen_server:cast(metrics_server, {reset_counter, Key}).

start_timer(Key) ->
    gen_server:cast(metrics_server, {start_timer, Key}).

get_timer(Key) ->
    gen_server:call(metrics_server, {get_timer, Key}).

append_gauge(Key, Value) ->
    gen_server:cast(metrics_server, {append_gauge, Key, Value}).

erase_gauge(Key) ->
    gen_server:cast(metrics_server, {erase_gauge, Key}).

get_gauge(Key) ->
    gen_server:call(metrics_server, {get_gauge, Key}).

min_max(Gauge) ->
    gen_server:call(metrics_server, {min_max, Gauge}).

mean(Gauge) ->
    gen_server:call(metrics_server, {mean, Gauge}).

%[TODO] percentile(Gauge, Percentile), median(Gauge)

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

incr_test() ->
    metrics_server:start_link(),
    start_timer(test),
    incr_counter(plop, 42),
    ?assertEqual(42, get_counter(plop)),
    incr_counter(plop, 1),
    ?assertEqual(43, get_counter(plop)),
    reset_counter(plop),
    incr_counter(plop, 3),
    ?assertEqual(3, get_counter(plop)),
    ?assert(get_timer(test) < 100).

gauge_test() ->
    append_gauge(truc, 42),
    append_gauge(truc, 40),
    append_gauge(truc, 44),
    ?assertEqual([42, 40, 44], get_gauge(truc)),
    ?assertEqual({40, 44}, min_max(truc)),
    ?assertEqual(42.0, mean(truc)),
    erase_gauge(truc),
    append_gauge(truc, 4807),
    ?assertEqual([4807], get_gauge(truc)).
-endif.