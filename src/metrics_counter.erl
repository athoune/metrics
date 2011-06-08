-module(metrics_counter).
-author('mathieu@garambrogne.net').

-export([
    incr/2,
    get/1,
    reset/1,
    to_list/0
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

incr(Counter, Inc) ->
    gen_server:cast(metrics_server, {incr_counter, Counter, Inc}).

get(Counter) ->
    gen_server:call(metrics_server, {get_counter, Counter}).

reset(Counter) ->
    gen_server:call(metrics_server, {reset_counter, Counter}).

to_list() ->
    gen_server:call(metrics_server, {list_counter}).