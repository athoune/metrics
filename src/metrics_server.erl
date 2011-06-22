-module(metrics_server).
-author('mathieu@garambrogne.net').

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-export([dump/1]).

-record(state, {start_time, counter, gauge, timer, writer}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    {ok, Store} = application:get_env(metrics, store),
    {ok, Period} = application:get_env(metrics, period),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Store, Period], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Writer, Period]) ->
    error_logger:info_msg("Metrics starts with ~s / ~wms", [Writer, Period]),
    %{ok, Tref} = timer:apply_interval(Period, gen_server, cast, [?MODULE, {flush}]),
    {ok, #state{
        start_time = now(),
        counter = dict:new(),
        gauge   = dict:new(),
     %   timer   = Tref,
        writer  = Writer
    }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_counter, Key}, _From, State) ->
    {reply, dict:fetch(Key, State#state.counter), State};
handle_call({get_gauge, Key}, _From, State) ->
    {reply, dict:fetch(Key, State#state.gauge), State};
handle_call({to_list_gauge}, _From, State) ->
    {reply, compute_gauge(State#state.gauge), State};
handle_call({min_max, Gauge}, _From, State) ->
    {reply,
        metrics_math:min_max(dict:fetch(Gauge, State#state.gauge)),
        State};
handle_call({mean, Gauge}, _From, State) ->
    {reply, metrics_math:mean(dict:fetch(Gauge, State#state.gauge)), State};
handle_call({percentile, Gauge, Percentile}, _From, State) ->
    {reply,
        metrics_math:percentile(dict:fetch(Gauge, State#state.gauge), Percentile),
        State};
handle_call({list_counter}, _From, State) ->
    {reply, dict:to_list(State#state.counter), State};
handle_call({snapshot}, _From, State) ->
    {Start, End, Counters, Gauges, NewState } = snapshot(State),
    {reply, {Start, End, Counters, Gauges}, NewState};
handle_call(_Request, _From, State) ->
    {reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({incr_counter, Key, Incr}, State) ->
    {noreply, State#state{
        counter = dict:update_counter(Key, Incr, State#state.counter)
    }};
handle_cast({reset_counter, Key}, State) ->
    {noreply, State#state{
        counter = dict:store(Key, 0, State#state.counter)
    }};
handle_cast({append_gauge, Key, Value}, State) when is_list(Value) ->
    {noreply, State#state{
        gauge = dict:append_list(Key, Value, State#state.gauge)
    }};
handle_cast({append_gauge, Key, Value}, State) ->
    {noreply, State#state{
        gauge = dict:append(Key, Value, State#state.gauge)
    }};
handle_cast({erase_gauge, Key}, State) ->
    {noreply, State#state{
        gauge = dict:store(Key, [], State#state.gauge)
    }};
handle_cast({flush},#state{writer = Writer} = State) ->
    {Start, End, Counters, Gauges, _NewState } = snapshot(State),
    Writer:write(Start, End, Counters, Gauges),
    {noreply, State}; % NewState
handle_cast(Msg, State) ->
    error_logger:warning_msg("Cast missing pattern : ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

dump(Driver) ->
    {Start, End, Counters, Gauges} = gen_server:call(?MODULE, {snapshot}),
    Driver:write(Start, End, Counters, Gauges).

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
