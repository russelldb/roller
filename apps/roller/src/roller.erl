%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%%
%%% @end
%%% Created : 28 Aug 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(ops).

%% API
-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================
doit2() ->
    {ok, Sock} = gen_tcp:connect("localhost", 5331, [list, {packet, 0}, {nodelay, true}, {active, true}]),
    timer:sleep(1500),
    error_logger:info_msg("slept for ~p~n", [1500]),
    {Ticks, T1, T2} = tick_count(50, 0.35908404),
    ok = gen_tcp:send(Sock, "l"),
    ok = gen_tcp:send(Sock, [T1]),
    ok = gen_tcp:send(Sock, [T2]),
    ok = gen_tcp:send(Sock, "\r"),
    
    error_logger:info_msg("sent the length ~n", []),
    receive_data(Sock, [], Ticks).

receive_data(Sock, Buf, Ticks) ->
    receive
	{tcp, Sock, "ERROR receiving tick lengths\r"} ->
	    error_logger:error_msg("Length set failed", []),
	    gen_tcp:close(Sock);
	{tcp, Sock, "OK "++L } ->
	    error_logger:info_msg("Got ~p~n", [string:tokens(lists:flatten(L), "\r\0")]),
	    gen_tcp:send(Sock, "g\r"),
	    receive_data(Sock, [],  Ticks);
	{tcp, Sock, L} ->
	    %%error_logger:info_msg("Got ~p~n", [string:tokens(lists:flatten(L), "\r\0")]),
	    update(Buf, L, Ticks, Sock);
	{tcp_closed, Sock} ->
	    io:format("closed")
    end.

update([N|T]) when N =:= 48; N =:= 49; N =:= 50; N =:= 51 ->
    %% check next char : == spin reading
    %% f == win reading
;
update([116|T]) ->
    %% A time reading, the end of this cycle
;
update() ->

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

