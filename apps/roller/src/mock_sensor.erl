%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Behaves just like a plugged in OS sensor, so system can run without it
%%% Start with an Env that includes
%%% How many rollers to simulate and a race plan (average speed for each roller)
%%% States are started, listening, connected, ready_to_race (IE has received a race length), countdown, racing
%%% @end
%%% Created : 30 Sep 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(mock_sensor).

-behaviour(gen_fsm).

%% API
-export([start_link/1, stop/0, listen/0]).

%% gen_fsm callbacks
-export([init/1, started/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {rollers, roller_diameter_metres, length, port, race_plan, socket, millis=0, ticks={0, 0, 0, 0}, timer, countdown=4}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link(Env) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Env) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Env, []).

listen() ->
    error_logger:info_msg("Listen", []),
    gen_fsm:send_event(?SERVER, listen).


stop() -> gen_fsm:sync_send_all_state_event(?SERVER, stop).

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
init(Env) ->
    Rollers = proplists:get_value(rollers, Env, 2),
    Length = proplists:get_value(length, Env, 250),
    Port = proplists:get_value(port, Env,  5331),
    RacePlan = proplists:get_value(race_plan, Env, [{1, 45}, {2, 46}]),
    RollerDiameter = roller_maths:inches_to_metres(proplists:get_value(roller_diameter_inches, Env, 4.5)),
    RaceTicks = roller_maths:ticks(Length, RollerDiameter),
    erlang:statistics(wall_clock), %%Just to zero the wall clock
    Timer = timer:send_interval(250, ?SERVER, update),
    {ok, Sock} = start_tcp(Port),
    {ok, started, #state{rollers=Rollers, roller_diameter_metres=RollerDiameter, length=Length, race_plan=RacePlan, port=Port, socket=Sock, timer=Timer}}.

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
started(listen, #state{socket=Socket}=State) ->
    {ok, Sock} = gen_tcp:accept(Socket),
    {next_state, listening, State#state{socket=Sock}}.

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
handle_sync_event(stop, _From, _StateName, #state{socket=Socket}=State) ->
    gen_tcp:shutdown(Socket, read_write),
    error_logger:info_msg("Stopped the socket", []),
    {stop, normal, ok, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

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
handle_info({tcp, Socket, [108, High, Low, 13, 0]}, listening, #state{socket=Socket}=State) ->
    error_logger:info_msg("Got this from the roller_sensor, ~p ~p~n", [High, Low]),
    Ticks = roller_maths:chars_to_ticks(High, Low),
    gen_tcp:send(Socket, lists:flatten(["OK ", integer_to_list(Ticks), [13, 0]])),
    Length = roller_maths:ticks_to_length(Ticks, State#state.roller_diameter_metres),
    {next_state, ready_to_race, State#state{length=Length}};
handle_info({tcp, Socket, "g/r"}, ready_to_race, #state{socket=Socket}=State) ->
    timer:send_after(1000, ?SERVER, {countdown, State#state.countdown}),
    {next_state, countingdown, State};
handle_info(update, CurrentState,  #state{socket=Socket}=State) ->
    %% send millis since start of race and tick counts
    {_, Millis} = erlang:statistics(wall_clock),
    gen_tcp:send(Socket, update_msg(Millis)),
    {next_state, CurrentState, State};
handle_info({coundown, 0}, countingdown, State) ->
    {next_state, racing, State};
handle_info({countdown, Count}, countingdown, State) ->
    timer:send_after(1000, ?SERVER, {countdown, Count-1}),
    {next_state, countingdown, State};
handle_info({countdown, _}, CurrentState, State) ->
    {next_state, CurrentState, State};
handle_info({tcp, "s/r"}, _StateName, State) ->
    {next_state, ready_to_race, State};
handle_info({tcp_closed, _DeadSocket}, _StateName, State) ->
    error_logger:info_msg("Socket closed"),
    gen_fsm:send_event(?SERVER, listen),
    {next_state, started, State}.
    
  
    

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
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    gen_tcp:shutdown(Socket, read_write).

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
start_tcp(Port) ->
    gen_tcp:listen(Port, [list]).

%% Pure, so move to pure module
update_msg(Millis) ->
    lists:flatten(["t: ", integer_to_list(Millis), "\r"]).
    
