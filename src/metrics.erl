-module(metrics).
-author('mathieu@garambrogne.net').

-export([
    incr_counter/2,
    get_counter/1,
    reset_counter/1,
    start_timer/1,
    get_timer/1
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
-endif.