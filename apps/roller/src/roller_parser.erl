%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2011, Russell Brown
%%% @doc
%%% parses roller command lists
%%% @end
%%% Created : 15 Jan 2011 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(roller_parser).

-compile(export_all).


parse(Command) ->
    parse(Command, []).


parse([], Acc) ->
    lists:reverse(Acc);
parse([0|T], Acc) ->
    parse(T, Acc);
parse([108, High, Low, 13|Rest], Acc) ->
    parse(Rest, [{length, roller_maths:chars_to_ticks(High, Low)}|Acc]);
parse([$g|T], Acc) ->
    parse(T, [{go}|Acc]);
parse([$s|T], Acc) ->
    parse(T, [{stop}|Acc]);
parse([$v|T], Acc) ->    
    parse(T, [{version}|Acc]).

