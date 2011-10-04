-module(metrics_app).
-author('mathieu@garambrogne.net').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = metrics_sup:start_link(),
    %metrics_error_handler:register_with_logger(),
    {ok, Pid}.

stop(_State) ->
    ok.
