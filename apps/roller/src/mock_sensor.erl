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
-define(UPDATE_FREQ_MILLI, 250).

-record(state, {rollers, roller_diameter_metres, length, port, race_plan, listen_socket, accept_socket, race_start_millis=0, ticks=[0, 0, 0, 0], timer, countdown=4}).

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
    error_logger:info_msg("Starting with Env = ~p~n", [Env]),
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Env, []).

listen() ->
    error_logger:info_msg("Listen~n", []),
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
    {ok, ListenSock} = start_tcp(Port),
    {ok, started, #state{rollers=Rollers, roller_diameter_metres=RollerDiameter, length=Length, race_plan=RacePlan, port=Port, listen_socket=ListenSock}}.

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
started(listen, #state{listen_socket=Socket}=State) ->
    {ok, Sock} = gen_tcp:accept(Socket),
    {next_state, listening, State#state{accept_socket=Sock}}.

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
handle_sync_event(stop, _From, _StateName, #state{listen_socket=Socket}=State) ->
    gen_tcp:close(Socket),
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
handle_info(update, racing,  #state{accept_socket=Socket, race_start_millis=Rsm, ticks=Ticks, roller_diameter_metres=RollerDiamMetres, race_plan=RacePlan}=State) ->
    %% send millis since start of race and tick counts
    {WC, _} = erlang:statistics(wall_clock),
    Millis = WC - Rsm,
    Ticks2 = ticks(Ticks, RacePlan, RollerDiamMetres, []),
    %% Must send the finished message and 
    %% if all racers a finished set the sensor to finished
    gen_tcp:send(Socket, update_msg(Millis, Ticks2)),
    {next_state, racing, State};
handle_info(update, S, State) ->
    {next_state, S, State};
handle_info({coundown, 0}, countingdown, State) ->
    Timer = timer:send_interval(?UPDATE_FREQ_MILLI, ?SERVER, update),
    {WC, _} = erlang:statistics(wall_clock),
    {next_state, racing, State#state{timer=Timer, race_start_millis=WC}};
handle_info({countdown, Count}, countingdown, State) ->
    timer:send_after(1000, ?SERVER, {countdown, Count-1}),
    {next_state, countingdown, State};
handle_info({countdown, _}, CurrentState, State) ->
    {next_state, CurrentState, State};
handle_info({tcp_closed, _Socket}, _StateName, State) ->
    error_logger:info_msg("Socket closed"),
    listen(),
    {next_state, started, State};
handle_info({tcp, _Socket, Mess}, State, Context) ->
    error_logger:info_msg("Got message ~p~nIn state ~p~n", [Mess, State]),
    Commands = roller_parser:parse(Mess),
    {NewState, NewContext} = handle_commands(Commands, State, Context),
    error_logger:info_msg("Commands are ~p~n", [Commands]),
    {next_state, NewState, NewContext}.

handle_commands([], State, Context) ->
    {State, Context};
handle_commands([{length, Ticks}|T], State, Context) ->
    Resp = lists:flatten(["OK ", integer_to_list(Ticks), 13, 0]),
    gen_tcp:send(Context#state.accept_socket, Resp),
    error_logger:info_msg("Got length ~p sending ~p~n", [Ticks, Resp]),
    Length = roller_maths:ticks_to_length(Ticks, Context#state.roller_diameter_metres),
    NextState = case State of
		    listening -> ready_to_race;
		    _ -> State
		end,
    handle_commands(T, NextState, Context#state{length=Length});
handle_commands([{go}|T], State, Context) ->
    timer:send_after(1000, ?SERVER, {countdown, Context#state.countdown}),
    handle_commands(T, State, Context);
handle_commands([{stop}|T], racing, #state{timer=Timer}=Context) ->
    timer:cancel(Timer),
    handle_commands(T, ready_to_race, Context#state{timer=undefined});
handle_commands([{stop}|T], _State, Context) ->
    error_logger:info_msg("Got stop~n"),
    handle_commands(T, ready_to_race, Context);
handle_commands([{version}|T], State, Context) ->
    error_logger:info_msg("Got version sending ~p~n", ["basic-1"]),
    gen_tcp:send(Context#state.accept_socket, lists:flatten(["basic-1", 13])),
    handle_commands(T, State, Context).
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
terminate(_Reason, _StateName, #state{listen_socket=Socket}) ->
    gen_tcp:close(Socket).

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
    gen_tcp:listen(Port, [list, {exit_on_close, false}]).

ticks([], [], _, NewTicks) ->
    lists:reverse(NewTicks);
ticks([OldTicks|Ticks], [], RollerDiamMetres, NewTicks) ->
    ticks(Ticks, [], RollerDiamMetres, [OldTicks|NewTicks]);
ticks([OldTicks|Ticks], [{_, MPH}|RestPlan], RollerDiamMetres, NewTicks) ->
    TicksPerSec = roller_math:ticks_per_second(RollerDiamMetres, MPH),
    RiderTicks = (TicksPerSec / 1000) * ?UPDATE_FREQ_MILLI,
    ticks(Ticks, RestPlan, RollerDiamMetres, [(RiderTicks + OldTicks)|NewTicks]).

%% Pure, so move to pure module
update_msg(Millis, Ticks) ->
    tick_msg(Ticks, []),
    lists:flatten(["t: ", integer_to_list(Millis), "\r"]).


tick_msg([], Mess) ->
    lists:reverse(Mess);
tick_msg([H|T], Mess) ->
    %% DO THIS NEXT
    ok.
	    

