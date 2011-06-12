-module(metrics_counter).
-author('mathieu@garambrogne.net').

-export([
    incr/1,
    incr/2,
    get/1,
    reset/1,
    to_list/0
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec incr(any()) -> 'ok'.
incr(Counter) ->
    gen_server:cast(metrics_server, {incr_counter, Counter, 1}).

-spec incr(any(), integer()) -> 'ok'.
incr(Counter, Inc) ->
    gen_server:cast(metrics_server, {incr_counter, Counter, Inc}).

-spec get(any()) -> integer().
get(Counter) ->
    gen_server:call(metrics_server, {get_counter, Counter}).

-spec reset(any()) -> 'ok'.
reset(Counter) ->
    gen_server:cast(metrics_server, {reset_counter, Counter}).

-spec to_list() -> list(tuple(any(), integer())).
to_list() ->
    gen_server:call(metrics_server, {list_counter}).