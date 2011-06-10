-module(metrics_timer).
-author('mathieu@garambrogne.net').

-export([
    start/1,
    get/1,
    reset/1,
    get_and_reset/1,
    exist/1
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

start(Key) ->
    gen_server:cast(metrics_server, {start_timer, Key}).

get(Key) ->
    gen_server:call(metrics_server, {get_timer, Key}).

reset(Key) ->
    gen_server:call(metrics_server, {reset_timer, Key}).

get_and_reset(Key) ->
    gen_server:call(metrics_server, {get_and_reset_timer, Key}).

exist(Key) ->
    gen_server:call(metrics_server, {exist_timer, Key}).
