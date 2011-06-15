-module(metrics_server).
-author('mathieu@garambrogne.net').

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-record(state, {counter, gauge}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{
        counter = dict:new(),
        gauge   = dict:new()
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
    {reply,
    dict:fold(
        fun(Key, Values, AccIn) ->
            {Min, Max} = min_max(Values),
            AccIn ++ [{Key, percentile(Values, 50), Min, Max}]
        end,
    [], State#state.gauge)
    , State};
handle_call({min_max, Gauge}, _From, State) ->
    {reply, min_max(dict:fetch(Gauge, State#state.gauge)), State};
handle_call({mean, Gauge}, _From, State) ->
    {reply, mean(dict:fetch(Gauge, State#state.gauge)), State};
handle_call({percentile, Gauge, Percentile}, _From, State) ->
    {reply, percentile(dict:fetch(Gauge, State#state.gauge), Percentile), State};
handle_call({list_counter}, _From, State) ->
    {reply, dict:to_list(State#state.counter),State};
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
handle_cast({flush}, State) ->
    metrics_gauge:to_file(),
    metrics_counter:to_file(),
    {noreply, State};
handle_cast(_Msg, State) ->
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
%% Private API
%%--------------------------------------------------------------------

mean(Values) ->
    Sum = lists:foldl(fun(T, Acc) ->
            T+Acc
        end, 0, Values),
    Sum / length(Values).

min_max(Values) ->
    [Head | Tail] = Values,
    {Min, Max} = lists:foldl(fun(T, {Lmin, Lmax}) ->
        Tmin = case T < Lmin of
            true -> T;
            _ -> Lmin
        end,
        Tmax = case T > Lmax of
            true -> T;
            _ -> Lmax
        end,
        {Tmin, Tmax}
        end, {Head, Head}, Tail),
    {Min, Max}.

percentile(Values, Percentile) ->
    G = lists:sort(Values),
    case Percentile of
        100 ->
            lists:last(G);
        _ ->
            lists:nth(round(length(G)  * Percentile / 100 + 0.5), G)
    end.