-module(metrics_gauge).
-author('mathieu@garambrogne.net').

-export([
    append/2,
    append_timer/2,
    erase/1,
    get/1,
    to_list/0,
    min_max/1,
    mean/1,
    percentile/2
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec append(any(), number()) -> none().
append(Key, Value) ->
    gen_server:cast(metrics_server, {append_gauge, Key, Value}).

-spec append_timer(any(), calendar:t_now()) -> none().
append_timer(Key, Starting) ->
    gen_server:cast(metrics_server, {append_gauge, Key, timer:now_diff(now(), Starting)}).

-spec erase(any()) -> none().
erase(Key) ->
    gen_server:cast(metrics_server, {erase_gauge, Key}).

-spec get(any()) -> list(number()).
get(Key) ->
    gen_server:call(metrics_server, {get_gauge, Key}).

-spec to_list() -> list(tuple(any(), list(number()))).
to_list() ->
    gen_server:call(metrics_server, {to_list_gauge}).

-spec min_max(any()) -> tuple(number(), number()).
min_max(Gauge) ->
    gen_server:call(metrics_server, {min_max, Gauge}).

-spec mean(any()) -> float().
mean(Gauge) ->
    gen_server:call(metrics_server, {mean, Gauge}).

%% http://en.wikipedia.org/wiki/Percentile
%% 0 <= Percentile <= 100
%% Percentile = 50 => median
-spec percentile(any(), number()) -> number().
percentile(Gauge, Percentile) ->
    gen_server:call(metrics_server, {percentile, Gauge, Percentile}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
    gauge_test() ->
        metrics_server:start_link(),
        metrics_gauge:append(truc, 42),
        metrics_gauge:append(truc, 40),
        metrics_gauge:append(truc, 44),
        ?assertEqual([42, 40, 44], metrics_gauge:get(truc)),
        ?assertEqual({40, 44}, min_max(truc)),
        ?assertEqual(42.0, mean(truc)),
        metrics_gauge:erase(truc),
        append(truc, 4807),
        ?assertEqual([4807], metrics_gauge:get(truc)),
        metrics_gauge:erase(truc),
        metrics_gauge:append(truc, [1,2,3,4,5,6,7,8,9,10]),
        ?assertEqual(6, percentile(truc, 50)),
        ?assertEqual(10, percentile(truc, 100)),
        ?assertEqual(1, percentile(truc, 0)).
-endif.