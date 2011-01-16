%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% A crack at a logging proxy so I can see what goldsprints and the os hardware are saying to each other.
%%% @end
%%% Created : 30 Sep 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(logproxy).

-behaviour(gen_fsm).

%% API
-export([start_link/1, stop/0, listen/0, mock_mode/0, version/0]).

%% gen_fsm callbacks
-export([init/1, started/2, listening/2, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {listen_socket, accept_socket, serial_socket}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Env) ->
    error_logger:info_msg("Starting with Env = ~p~n", [Env]),
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Env, []).

listen() ->
    error_logger:info_msg("Listen~n", []),
    gen_fsm:send_event(?SERVER, listen).

version() ->
    error_logger:info_msg("Version~n"),
    gen_fsm:send_event(?SERVER, version).

mock_mode() ->
    error_logger:info_msg("Go MOCK!~n"),
    gen_fsm:send_event(?SERVER, mock).

stop() -> 
    gen_fsm:sync_send_all_state_event(?SERVER, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init(Env) ->
    SerialPort = proplists:get_value(serial_port, Env,  5332),
    ListenPort = proplists:get_value(listen_port, Env, 5331),
    {ok, SerialSock} = gen_tcp:connect("localhost", SerialPort, [list, {packet, 0}, {nodelay, true}, {active, true}]),
    {ok, ListenSock} = start_tcp(ListenPort),
    {ok, started, #state{serial_socket=SerialSock, listen_socket=ListenSock}}.

started(listen, #state{listen_socket=Socket}=State) ->
    {ok, Sock} = gen_tcp:accept(Socket),
    {next_state, listening, State#state{accept_socket=Sock}}.

listening(mock, #state{serial_socket=Socket}=State) ->
    gen_tcp:send(Socket, [$m]),
    error_logger:info_msg("Toggled mock mode~n"),
    {next_state, listening, State};
listening(version, #state{serial_socket=Socket}=State) ->
    gen_tcp:send(Socket, [$v]),
    error_logger:info_msg("Aksed for version~n"),
    {next_state, listening, State}.

handle_sync_event(stop, _From, _StateName, #state{listen_socket=Socket}=State) ->
    gen_tcp:close(Socket),
    error_logger:info_msg("Stopped the socket", []),
    {stop, normal, ok, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({tcp_closed, _Socket}, _StateName, State) ->
    error_logger:info_msg("Socket closed"),
    listen(),
    {next_state, started, State};
handle_info({tcp, Socket, Mess}, State, Context) when Socket =:= Context#state.accept_socket ->
    error_logger:info_msg("Got message ~p from goldsprints~n", [Mess]),
    gen_tcp:send(Context#state.serial_socket, Mess),
    error_logger:info_msg("Sent message to Serial"),
    {next_state, State, Context};
handle_info({tcp, Socket, Mess}, State, Context) when Socket =:= Context#state.serial_socket ->
    error_logger:info_msg("Got message ~p from serial~n", [Mess]),
    gen_tcp:send(Context#state.accept_socket, Mess),
    error_logger:info_msg("Sent message to goldsprints"),
    {next_state, State, Context}.

terminate(_Reason, _StateName, #state{listen_socket=Socket}) ->
    gen_tcp:close(Socket).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_event(_E, SN, SD) ->
    {next_state, SN, SD}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_tcp(Port) ->
    gen_tcp:listen(Port, [list, {exit_on_close, false}]).
