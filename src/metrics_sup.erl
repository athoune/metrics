-module(metrics_sup).
-author('mathieu@garambrogne.net').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    {ok, Pid} = gen_event:start_link({local, metrics_event}),
    R.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
%       {metrics_server, {metrics_server, start_link, []},
%       permanent, 2000, worker, [metrics_server]
%       }
        %,?CHILD(metrics_csv, worker)
    ]} }.

