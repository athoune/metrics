-module(metrics_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
handle_info/2, terminate/2, code_change/3]).

-export([
    incr_counter/2,
    get_counter/1,
    reset_counter/1
]).

-record(state, {counter, timer}).

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
        timer = dict:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, Key}, _From, State) ->
    {reply, dict:fetch(Key, State#state.counter), State};
handle_call(_Request, _From, State) ->
    {reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({incr_counter, Key, Incr}, State) ->
    Count = case dict:is_key(Key, State#state.counter) of
        true ->
            dict:fetch(Key, State#state.counter) + Incr;
        _ ->
            Incr
    end,
    {noreply, State#state{
        counter = dict:store(Key, Count, State#state.counter)
    }};
handle_cast({reset_counter, Key}, State) ->
    {noreply, State#state{
        counter = dict:store(Key, 0, State#state.counter)
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


incr_counter(Key, Inc) ->
    gen_server:cast(?MODULE, {incr_counter, Key, Inc}).

get_counter(Key) ->
    gen_server:call(?MODULE, {get, Key}).

reset_counter(Key) ->
    gen_server:cast(?MODULE, {reset_counter, Key}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

incr_test() ->
    start_link(),
    incr_counter(plop, 42),
    ?assertEqual(42, get_counter(plop)),
    incr_counter(plop, 1),
    ?assertEqual(43, get_counter(plop)),
    reset_counter(plop),
    incr_counter(plop, 3),
    ?assertEqual(3, get_counter(plop)).
-endif.