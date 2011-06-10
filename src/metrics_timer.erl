-module(metrics_timer).
-author('mathieu@garambrogne.net').

-export([
    init/1,
    get/1,
    get_and_reset/1,
    exists/1
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

init(Key) ->
    gen_server:cast(metrics_server, {init_timer, Key}).

get(Key) ->
    gen_server:call(metrics_server, {get_timer, Key}).

get_and_reset(Key) ->
    gen_server:call(metrics_server, {get_and_reset_timer, Key}).

exists(Key) ->
    gen_server:call(metrics_server, {exists_timer, Key}).
