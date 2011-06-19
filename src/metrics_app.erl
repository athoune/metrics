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
    {ok, Writer} = application:get_env(metrics, store),
    supervisor:start_child(metrics_sup, {Writer,
        {Writer, start_link, []}, permanent, 5000, worker, [Writer]}),
    {ok, Pid}.

stop(_State) ->
    ok.
