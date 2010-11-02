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
      fun length_set/1,
      fun race_starts/1]}.

% Setup and Cleanup
setup() -> 
    {ok, Pid} = roller_sensor:start_link(), 
    {ok, MockPid} = mock_sensor:start_link([]),
    {ok, Controller} = roller_controller:start_link(),
    {Pid, MockPid, Controller}.

cleanup({_, MockPid, _}) -> 
    mock_sensor:stop(MockPid),
    roller_controller:stop(),
    roller_sensor:stop().

%% Tests
started_properly(_) ->
    fun() ->
            ?assertEqual(ready_to_connect, roller_sensor:introspection_statename()),
            ?assertEqual({state, undefined, undefined},
            roller_sensor:introspection_loopdata())
    end.

connected_ok(_) ->
    fun() ->
	    ?assertEqual(ok, roller_sensor:connect(5331)),
	    ?assertEqual(connected,  roller_sensor:introspection_statename()),
	    {state, Socket, undefined} = roller_sensor:introspection_loopdata(),
	    ?assert(is_port(Socket))
    end.
	    
length_set({_, MockPid, _}) ->
    fun() ->
	    mock_sensor:listen(MockPid),
	    ok = roller_sensor:connect(5331),
	    ?assertEqual({ok, 139}, roller_sensor:set_length( 50, 0.35908404)), %% distance of 50 metres and a diameter of 4.5 inches
	    ?assertEqual(length_set, roller_sensor:introspection_statename()),
	    {state, _, Ticks} = roller_sensor:introspection_loopdata(),
	    ?assertEqual(Ticks, 139)
    end.

race_starts({_, MockPid, _}) ->
    ok.
	    
	   
