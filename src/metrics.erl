-module(metrics).
-author('mathieu@garambrogne.net').

-export([
    snapshot/0,
    add_writer/1,
    add_writer/2,
    add_writer/3,
    erlang_metrics/0
]).

-spec snapshot() -> tuple(tuple(number(), number(), number()),
    tuple(number(), number(), number()), list(), list()).
snapshot() ->
    gen_server:call(metrics_server, {snapshot}).

% metrics_csv
add_writer(Writer) ->
    add_writer(Writer, 10000).
add_writer(Writer, Period) ->
    add_writer(Writer, Period, []).
add_writer(Writer, Period, Options) ->
    {ok, _} = supervisor:start_child(metrics_sup, {Writer,
        {Writer, start_link, Options}, permanent, 5000, worker, [Writer]}),
    gen_server:cast(metrics_server, {set_writer, Writer, Period}).

erlang_metrics() ->
    metrics_gauge:append("erlang:processes", erlang:system_info(process_count)),
    metrics_gauge:append("erlang:schedulers", erlang:system_info(schedulers)),
    {RunTime, _} = erlang:statistics(runtime),
    metrics_gauge:append("erlang:runtime", RunTime),
    metrics_gauge:append("erlang:run_queue", erlang:statistics(run_queue)),
    {{input, Rx}, {output, Tx}} = erlang:statistics(io),
    metrics_gauge:append("erlang:io:input", Rx),
    metrics_gauge:append("erlang:io:output", Tx).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

incr_test() ->
    application:start(metrics),
    ok = gen_event:add_handler(metrics_event, metrics_server, []),
    metrics_counter:incr(plop, 42),
    ?assertEqual(42, metrics_counter:get(plop)),
    metrics_counter:incr(plop, 1),
    ?assertEqual(43, metrics_counter:get(plop)),
    metrics_counter:reset(plop),
    metrics_counter:incr(plop, 3),
    ?assertEqual(3, metrics_counter:get(plop)).

-endif.
