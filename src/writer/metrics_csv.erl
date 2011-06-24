-module(metrics_csv).
-author('mathieu@garambrogne.net').

-behaviour(metrics_writer).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, start_link/1, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-export([write_gauge/2, write_counter/2, write/4]).

-record(state, {folder}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    start_link("/tmp").

start_link(Path) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Path], []).

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
init([Path]) ->
    error_logger:info_msg("Starting CSV server with path: ~s~n", [Path]),
    {ok, #state{
        folder = Path
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
handle_call(_Request, _From, State) ->
    {reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({write_gauge, Values, {Mega, Second, _}}, State) ->
    {ok, Fd} = file:open(
        io_lib:format("~s/gauge.~w~w.csv", [State#state.folder, Mega, Second]),
        [write]),
    ok = gauge_lines(Fd, Values),
    file:close(Fd),
    {noreply, State};
handle_cast({write_counter, Values, {Mega, Second, _}}, State) ->
    {ok, Fd} = file:open(
        io_lib:format("~s/counter.~w~w.csv", [State#state.folder, Mega, Second]),
        [write]),
    ok = counter_lines(Fd, Values),
    file:close(Fd),
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
%% Public API
%%--------------------------------------------------------------------

write_gauge(Values, Time) ->
    gen_server:cast(?MODULE, {write_gauge, Values, Time}).

write_counter(Values, Time) ->
    gen_server:cast(?MODULE, {write_counter, Values, Time}).

write(_StartTime, EndTime, Counters, Gauges) ->
    ok = write_counter(Counters, EndTime),
    write_gauge(Gauges, EndTime).

%%--------------------------------------------------------------------
%% Private API
%%--------------------------------------------------------------------

gauge_lines(_Fd, []) ->
    ok;
gauge_lines(Fd, [{Key, Median, Min, Max}|T]) ->
    file:write(Fd, io_lib:format("~s;~w;~w;~w~n", [Key, Median, Min, Max])),
    gauge_lines(Fd, T).

counter_lines(_Fd, []) ->
    ok;
counter_lines(Fd, [{Key, Value}|T]) ->
    file:write(Fd, io_lib:format("~s;~w~n", [Key, Value])),
    counter_lines(Fd, T).