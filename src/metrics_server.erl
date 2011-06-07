-module(metrics_server).
-author('mathieu@garambrogne.net').

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-record(state, {counter, timer, gauge}).

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
        timer   = dict:new(),
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
handle_call({get_timer, Key}, _From, State) ->
    {reply, timer:now_diff(now(), dict:fetch(Key, State#state.timer)), State};
handle_call({get_gauge, Key}, _From, State) ->
    {reply, dict:fetch(Key, State#state.gauge), State};
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
handle_cast({start_timer, Key}, State) ->
    {noreply, State#state{
        timer = dict:store(Key, now(), State#state.timer)
    }};
handle_cast({append_gauge, Key, Value}, State) ->
    {noreply, State#state{
        gauge = dict:append(Key, Value, State#state.gauge)
    }};
handle_cast({erase_gauge, Key}, State) ->
    {noreply, State#state{
        gauge = dict:erase(Key, State#state.gauge)
    }};
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