-module(metrics_counter).
-author('mathieu@garambrogne.net').

-export([
    incr/2,
    get/1,
    reset/1
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

incr(Key, Inc) ->
    gen_server:cast(metrics_server, {incr_counter, Key, Inc}).

get(Key) ->
    gen_server:call(metrics_server, {get_counter, Key}).

reset(Key) ->
    gen_server:call(metrics_server, {reset_counter, Key}).
