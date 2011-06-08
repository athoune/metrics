-module(metrics_gauge).
-author('mathieu@garambrogne.net').

-export([
    append/2,
    erase/1,
    get/1,
    min_max/1,
    mean/1
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

append(Key, Value) ->
    gen_server:cast(metrics_server, {append_gauge, Key, Value}).

erase(Key) ->
    gen_server:call(metrics_server, {erase_gauge, Key}).

get(Key) ->
    gen_server:call(metrics_server, {get_gauge, Key}).

min_max(Gauge) ->
    gen_server:call(metrics_server, {min_max, Gauge}).

mean(Gauge) ->
    gen_server:call(metrics_server, {mean, Gauge}).

%[TODO] percentile(Gauge, Percentile), median(Gauge)

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
        ?assertEqual([4807], metrics_gauge:get(truc)).

-endif.