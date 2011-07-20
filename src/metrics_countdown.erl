-module(metrics_countdown).

-author('mathieu@garambrogne.net').

-export([create/3, decr/1, decr/2]).

create(Key, Value, Fun) ->
    gen_server:cast(metrics_server, {create_countdown, Key, Value, Fun}).

decr(Key) ->
    gen_server:cast(metrics_server, {decr_countdown, Key, 1}).

decr(_Key, 0) ->
    ok;
decr(Key, Value) ->
    gen_server:cast(metrics_server, {decr_countdown, Key, Value}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

    decr_test() ->
        application:start(metrics),
        ok = metrics_countdown:create(plop, 2, fun(_Time) ->
            ok = metrics_counter:incr(countdown)
        end),
        ok = metrics_countdown:decr(plop),
        ok = metrics_countdown:decr(plop),
        timer:sleep(1), %this sequence is single threaded, give time to callback to execute
        ?assertEqual(1, metrics_counter:get(countdown)).

-endif.
