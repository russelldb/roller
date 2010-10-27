%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%%
%%% @end
%%% Created : 12 Sep 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(test_roller_sensor).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun started_properly/1,
      fun connected_ok/1,
      fun length_set/1]}.

% Setup and Cleanup
setup() -> 
    {ok, Pid} = roller_sensor:start_link(), 
    {ok, MockPid} = mock_sensor:start_link([]),
    {Pid, MockPid}.

cleanup({Pid, MockPid}) -> 
    mock_sensor:stop(MockPid),
    roller_sensor:stop(Pid).

%% Tests
started_properly({Pid, _}) ->
    fun() ->
            ?assertEqual(ready_to_connect, roller_sensor:introspection_statename(Pid)),
            ?assertEqual({state, undefined, undefined},
            roller_sensor:introspection_loopdata(Pid))
    end.

connected_ok({Pid, _}) ->
    fun() ->
	    ?assertEqual(ok, roller_sensor:connect(Pid, 5331)),
	    ?assertEqual(connected,  roller_sensor:introspection_statename(Pid)),
	    {state, Socket, undefined} = roller_sensor:introspection_loopdata(Pid),
	    ?assert(is_port(Socket))
    end.
	    
length_set({Pid, _}) ->
    fun() ->
	    ok = roller_sensor:connect(Pid, 5331),
	    ?assertEqual({ok, 139}, roller_sensor:set_length(Pid, 50, 0.35908404)), %% distance of 50 metres and a diameter of 4.5 inches
	    ?assertEqual(length_set, roller_sensor:introspection_statename(Pid)),
	    {state, _, Ticks} = roller_sensor:introspection_loopdata(Pid),
	    ?assertEqual(Ticks, 139)
    end.
	    
	   
