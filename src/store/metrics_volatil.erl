-module(metrics_volatil).
-author('mathieu@garambrogne.net').

-behaviour(gen_event).

%% gen_server callbacks
-export([init/1, handle_call/2, handle_event/2,
handle_info/2, terminate/2, code_change/3]).

-export([dump/1, snapshot/0, raw_snapshot/0]).

-record(state, {
    start_time,
    counter,
    gauge,
    countdown,
    timer,
    writer}).

init([]) ->
    init([0, none]);
init([_Refresh, Writer]) ->
    {ok, #state{
        start_time = now(),
        counter    = dict:new(),
        gauge      = dict:new(),
        countdown  = dict:new(),
        timer      = none,
        writer     = Writer
    }}.

handle_call({get_counter, Key}, State) ->
    {ok, dict:fetch(Key, State#state.counter), State};

handle_call({get_gauge, Key}, State) ->
    {ok, dict:fetch(Key, State#state.gauge), State};

handle_call({to_list_gauge}, State) ->
    {ok, compute_gauge(State#state.gauge), State};

handle_call({min_max, Gauge}, State) ->
    {ok,
        metrics_math:min_max(dict:fetch(Gauge, State#state.gauge)),
        State};

handle_call({mean, Gauge}, State) ->
    {ok, metrics_math:mean(dict:fetch(Gauge, State#state.gauge)), State};

handle_call({percentile, Gauge, Percentile}, State) ->
    {ok,
        metrics_math:percentile(dict:fetch(Gauge, State#state.gauge), Percentile),
        State};

handle_call({list_counter}, State) ->
    {ok, dict:to_list(State#state.counter), State};

handle_call({snapshot}, State) ->
    {Start, End, Counters, Gauges, NewState } = snapshot(State),
    {ok, {Start, End, Counters, Gauges}, NewState};

handle_call({raw_snapshot}, State) ->
    S = now(),
    {ok, {State#state.start_time, S, dict:to_list(State#state.counter), dict:to_list(State#state.gauge) },
        State#state{
            start_time = S,
            counter = dict:new(),
            gauge   = dict:new()
    }};

handle_call(_Request, State) ->
    {ok, State}.

handle_event({incr_counter, Key, Incr}, State) ->
    {ok, State#state{
        counter = dict:update_counter(Key, Incr, State#state.counter)
    }};

handle_event({reset_counter, Key}, State) ->
    {ok, State#state{
        counter = dict:store(Key, 0, State#state.counter)
    }};

handle_event({append_gauge, Key, Value}, State) when is_list(Value) ->
    {ok, State#state{
        gauge = dict:append_list(Key, Value, State#state.gauge)
    }};

handle_event({append_gauge, Key, Value}, State) ->
    {ok, State#state{
        gauge = dict:append(Key, Value, State#state.gauge)
    }};

handle_event({erase_gauge, Key}, State) ->
    {ok, State#state{
        gauge = dict:store(Key, [], State#state.gauge)
    }};

% FIXME
handle_event({flush},#state{writer = Writer} = State) ->
    metrics:erlang_metrics(),
    {Start, End, Counters, Gauges, NewState } = snapshot(State),
    Writer:write(Start, End, lists:sort(Counters), lists:sort(Gauges)),
    {ok, NewState};

% FIXME
handle_event({set_writer, Writer, Period}, State) ->
    error_logger:info_msg("Metrics starts with ~s / ~wms~n", [Writer, Period]),
    {ok, Tref} = case Period of
        0 -> {ok, none};
        _ -> timer:apply_interval(Period, gen_server, cast, [?MODULE, {flush}])
    end,
    {ok, State#state{
        timer   = Tref,
        writer  = Writer
    }};

handle_event({create_countdown, Key, Value, Fun}, State) ->
    {ok, State#state{
        countdown = dict:store(Key, {Value, Fun, now()}, State#state.countdown)
    }};

handle_event({decr_countdown, Key, Step}, State) ->
    {Value, Fun, StartTime} = dict:fetch(Key, State#state.countdown),
    case (Value rem 100) of
        0 ->
            error_logger:info_msg("counting : ~w 10k~n", [Value / 10000]);
        _ ->
            nop
    end,
    case Value of
        Step ->
            Fun(timer:now_diff(now(), StartTime)),
            {ok, State#state{
                countdown = dict:erase(Key, State#state.countdown)
            }};
        _ ->
            {ok, State#state{
                countdown = dict:store(Key, {Value - Step, Fun, StartTime}, State#state.countdown)
            }}
    end;

handle_event(Msg, State) ->
    error_logger:warning_msg("Cast missing pattern : ~p~n", [Msg]),
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

dump(Driver) ->
    {Start, End, Counters, Gauges} = gen_event:call(metrics_event, ?MODULE, {snapshot}),
    Driver:write(Start, End, Counters, Gauges).

snapshot() ->
    gen_event:call(metrics_event, ?MODULE, {snapshot}).

raw_snapshot() ->
    gen_event:call(metrics_event, ?MODULE, {raw_snapshot}).

%%--------------------------------------------------------------------
%% Private API
%%--------------------------------------------------------------------

compute_gauge(Gauges) ->
    dict:fold(
        fun(Key, Values, AccIn) ->
            {Min, Max} = metrics_math:min_max(Values),
            AccIn ++ [{Key, metrics_math:percentile(Values, 50), Min, Max}]
        end,
    [], Gauges).

snapshot(State) ->
    C = State#state.counter,
    G = State#state.gauge,
    S = now(),
    {State#state.start_time, S, dict:to_list(C), compute_gauge(G), State#state{
        start_time = S,
        counter = dict:new(),
        gauge   = dict:new()
    }}.
