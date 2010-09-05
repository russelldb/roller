%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%%
%%% @end
%%% Created : 28 Aug 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(roller_web).

%% API
-export([start/0, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================
%% stop misultin
stop() ->
    misultin:stop().

start() ->
    %% Read config...
    {ok, BindAddress} = application:get_env(roller, bind_address),
    {ok, Port} = application:get_env(roller, port),
    {ok, DocRoot} = application:get_env(roller, document_root),

    %% All this socket shit should be a gen_server. Then each client WS can register as a pid
    %% The gen server can through messages at them all As it is each websocket will try to "own" the socket
    SockOwner = spawn(fun() -> sock_owner(undefined) end),
    SockOwner ! connect,
    
    Options = [
        {ip, BindAddress}, 
        {port, Port},
        {loop, fun(Req) -> handle_http(Req, DocRoot) end},
	{ws_loop, fun(Ws) -> handle_websocket(Ws, SockOwner) end},
	{ws_autoexit, true}
    ],

    misultin:start_link(Options).


handle_http(Req, DocRoot) ->
    error_logger:info_msg("Got request~n", []),
    Method = Req:get(method),
    error_logger:info_msg("Got method ~p~n", [Method]),
    {UriType, Uri} = Req:get(uri),
    error_logger:info_msg("Got path ~p ~p~n", [UriType, Uri]),
    handle_http(Method, Uri, Req, DocRoot).

handle_http('GET', "/" ++ Path, Req, DocRoot) ->
    View = get_view(Path),
    case tempile:render(View, [{current, View}]) of
	{ok, RespBody} ->
	    Req:respond(200, [{"Content-Type", "text/html; charset=UTF-8"}], RespBody);
	_ ->
	    Req:file(DocRoot ++ Path)
    end;
handle_http('POST', _Path, Req, _DocRoot) ->
    Req:respond(405, [{"Allow", "GET"}, {"Content-Type", "text/plain"}], "Method not allowed");
handle_http(_, _, Req, _DocRoot) ->
    Req:respond(405, [{"Allow", "GET"}, {"Content-Type", "text/plain"}], "Method not allowed").


% callback on received websockets data
handle_websocket(Ws, Sock) ->
    case is_pid(Sock) of
	true ->
	    error_logger:info_msg("I asks for the socket~n", []),
	    Sock ! {take_socket, self()},
	    receive
		{socket, S} ->
		    error_logger:info_msg("I gets the socket~n", []),
		    inet:setopts(S, [{active, true}]),
		    handle_websocket(Ws, S)
	    end;
	false -> 
	    receive
		{browser, Data} ->
		    Ws:send(["received '", Data, "'"]),
		    gen_tcp:send(Sock, "g\r"),
		    handle_websocket(Ws, Sock);
		{tcp, Sock, L} ->
		    Ws:send(L),
		    handle_websocket(Ws, Sock);
		{tcp_closed, Sock} ->
		    io:format("closed");
		{socket, S} ->
		    error_logger:info_msg("I gets the socket~n", []),
		    handle_websocket(Ws, S);
		_Ignore ->
		    handle_websocket(Ws, Sock)
	    end
    end.

%% Figure out the view name
get_view("") ->
	"index";
get_view(Path) ->
	filename:rootname(filename:basename(Path)).

sock_owner(S) ->
    receive
	connect ->
	    {ok, Sock} = gen_tcp:connect("localhost", 5331, [list, {packet, 0}, {nodelay, true}, {active, false}]),

	    timer:sleep(1500),
	    error_logger:info_msg("slept for ~p~n", [1500]),
	    {Ticks, T1, T2} = tick_count(50, 0.35908404),
	    ok = gen_tcp:send(Sock, "l"),
	    ok = gen_tcp:send(Sock, [T1]),
	    ok = gen_tcp:send(Sock, [T2]),
	    ok = gen_tcp:send(Sock, "\r"),

	    error_logger:info_msg("sent the length ~p~n", [Ticks]),

	    {ok, "OK "++L} = gen_tcp:recv(Sock, 0, 500),
	    error_logger:info_msg("Got ~p~n", [string:tokens(lists:flatten(L), "\r\0")]),
	    sock_owner(Sock);
	{take_socket, Pid} ->
	    error_logger:info_msg("He asks for the socket, I gives him the socket ~p ~p~n", [is_pid(Pid), is_port(S)]),
	    ok = gen_tcp:controlling_process(S, Pid),
	    Pid ! {socket, S}
    end.

%%--------------------------------------------------------------------
%% @doc generates the 2 char list that represents the number of ticks for a race
%% @spec tick_count(int(), float()) -> {char(), char()}.
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
