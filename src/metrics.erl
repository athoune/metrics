-module(metrics).
-author('mathieu@garambrogne.net').

-export([
    snapshot/0,
    add_writer/1,
    add_writer/2,
    erlang_metrics/0
]).

% @deprecated
-spec snapshot() -> tuple(tuple(number(), number(), number()),
    tuple(number(), number(), number()), list(), list()).
snapshot() ->
    metrics_volatil:snapshot().

add_writer(Writer) ->
    add_writer(Writer, []).

add_writer(Writer, Args) ->
    % [TODO] attach to the supervisor
    ok = gen_event:add_handler(metrics_event, Writer, Args).

% Fetch infos from the erlang runtime
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

setup_test_() ->
    {spawn,
        [{setup,
            fun metrics_test:setup/0,
            fun metrics_test:cleanup/1,

            fun() ->
                add_writer(metrics_volatil),
                metrics_counter:incr(plop, 42),
                ?assertEqual(42, metrics_counter:get(plop)),
                metrics_counter:incr(plop, 1),
                ?assertEqual(43, metrics_counter:get(plop)),
                metrics_counter:reset(plop),
                metrics_counter:incr(plop, 3),
                ?assertEqual(3, metrics_counter:get(plop))
            end
        }]}.

-endif.
