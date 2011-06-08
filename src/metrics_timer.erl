-module(metrics_timer).
-author('mathieu@garambrogne.net').

-export([
    start/1,
    get/1
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

start(Key) ->
    gen_server:cast(metrics_server, {start_timer, Key}).

get(Key) ->
    gen_server:call(metrics_server, {get_timer, Key}).
