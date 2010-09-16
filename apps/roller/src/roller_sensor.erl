%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Models the sensor hardware
%%% @end
%%% Created : 11 Sep 2010 by Russell Brown <russell@ossme.net>
%% States are DISCONNECTED -> CONNECTED -> LENGTH_SET -> RACING
%% DISCONNECTED -connect-> CONNECTED -set length-> LENGTH_SET -race-> RACING -stop->LENGTH_SET
%%%-------------------------------------------------------------------
-module(roller_sensor).

-behaviour(gen_fsm).

%% API
-export([start_link/0, stop/1, connect/2, set_length/3, race/1, stop_race/1, disconnect/1]).

%% Introspection
-export([introspection_statename/1, introspection_loopdata/1]).

%% States
-export([ready_to_connect/2, ready_to_connect/3, connected/2, connected/3, length_set/2, length_set/3, racing/2, racing/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {socket, ticks}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

introspection_statename(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, which_statename).

introspection_loopdata(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, which_loopdata).

stop(Pid) -> gen_fsm:sync_send_all_state_event(Pid,stop).

connect(Pid, Port) ->
    gen_fsm:sync_send_event(Pid, {connect, Port}).

set_length(Pid, Metres, RollerDiameter) when is_integer(Metres) ->
    gen_fsm:sync_send_event(Pid, {set_length, Metres, RollerDiameter}).

race(Pid) ->
    gen_fsm:send_event(Pid, race).
    
stop_race(Pid) ->
    gen_fsm:send_event(Pid, stop_race).

disconnect(Pid) ->
    gen_fsm:send_event(Pid, disconnect).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok,  ready_to_connect, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
ready_to_connect(_E, State) ->
    {next_state, ready_to_connect, State}.


connected(disconnect, #state{socket=Socket}=State) ->
    ok = gen_tcp:close(Socket),
    {next_state, disconnected, State#state{socket=undefined}};
connected(_E, State) ->
    {next_state, connected, State}.

length_set(race, #state{socket=Socket}=State) ->
    ok = inet:set_opts(Socket, {active, true}),
    ok = gen_tcp:send(Socket, "g"),
    {next_state, racing, State};
length_set(disconnect, #state{socket=Socket}=State) ->
    ok = gen_tcp:close(Socket),
    {next_state, disconnected, State#state{socket=undefined}}.

racing(stop_race, #state{socket=Socket}=State) ->
    ok = gen_tcp:send(Socket, "s"),
    {next_state, length_set, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
ready_to_connect({connect, Port}, _From, State) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [list, {packet, 0}, {nodelay, true}, {active, false}]),
    timer:sleep(1500),
    {reply, ok, connected, State#state{socket=Sock}}.

connected({set_length, Metres, RollerDiameter}, _From, State) when is_integer(Metres)->
    {Ticks, TickHigh, TickLow} = tick_count(Metres, RollerDiameter),
    Sock = State#state.socket,
    ok = gen_tcp:send(Sock, "l"),
    ok = gen_tcp:send(Sock, [TickHigh]),
    ok = gen_tcp:send(Sock, [TickLow]),
    ok = gen_tcp:send(Sock, "\r"),

    error_logger:info_msg("sent the length ~p~n", [Ticks]),

    TickMatch = integer_to_list(Ticks) ++ [13, 0],

    {ok, "OK "++TickMatch} = gen_tcp:recv(Sock, 0, 500),
    {reply, {ok, Ticks}, length_set, State#state{ticks=Ticks}};
connected(disconnect, _From, State) ->
    {reply, ok, connected, State}.

length_set(race, _From, State) ->
    {reply, ok, length_set, State};
length_set(disconnect, _From, State) ->
    {reply, ok, length_set, State}.
    
racing(stop_race, _From, State) ->
    {reply, ok, racing, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(which_statename, _From, StateName, LoopData) ->
    {reply, StateName, StateName, LoopData};
handle_sync_event(which_loopdata, _From, StateName, LoopData) ->
    {reply, LoopData, StateName, LoopData};
handle_sync_event(stop,_From,_StateName,LoopData) ->
    {stop, normal, ok, LoopData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, L}, racing, State) ->
    %%What to do?? This is where we get data from the os hardware and send it on to websockets or whatever
    {next_state, racing, State};
handle_info({tcp_closed, Sock}, racing, State) ->
    %%What to do???
    {next_state, disconnected, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc generates the 2 char list that represents the number of ticks for a race
%% @spec tick_count(int(), float()) -> {int(), char(), char()}.
%% @end
%%--------------------------------------------------------------------
tick_count(RaceLength, RollerCircumference) ->
    Ticks = floor(RaceLength / RollerCircumference),
    {Ticks, Ticks rem 256, Ticks div 256}.    
%%--------------------------------------------------------------------
%% @doc finds to floor of X
%% @spec floor(X::float()) -> int().
%% @end
%%--------------------------------------------------------------------
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.
