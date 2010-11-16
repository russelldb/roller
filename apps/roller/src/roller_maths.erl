%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% All the functions for figuring stuff out.
%%% @end
%%% Created : 10 Nov 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(roller_maths).

%% API
-export([ticks/2, tick_chars/2, tick_chars/1, inches_to_metres/1, chars_to_ticks/2, ticks_to_length/2, roller_circ/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc How many ticks for a race of RaceLength with roller circumference in METRES
%% @spec ticks(RaceLength::int(), RollerCirc::float()) -> Ticks:int()
%% @end
%%--------------------------------------------------------------------
ticks(RaceLength, RollerCirc) ->
    floor(RaceLength / RollerCirc).

%%--------------------------------------------------------------------
%% @doc generates the 2 char tuple that represents the number of ticks for a race
%% @spec tick_count(RaceLength::int(), RollerDiameter::float()) -> {Ticks::int(), char(), char()}.
%% @end
%%--------------------------------------------------------------------
tick_chars(RaceLength, RollerCirc) ->
    Ticks = ticks(RaceLength,  RollerCirc),
    tick_chars(Ticks).

%%--------------------------------------------------------------------
%% @doc generates the 3 tuple that represents the number of ticks for a race. 
%% This is how the arduino accepts the ticks.
%% @spec tick_count(Ticks::int) -> {Ticks::int(), char(), char()}.
%% @end
%%--------------------------------------------------------------------
tick_chars(Ticks) ->
    {Ticks, Ticks rem 256, Ticks div 256}.   

%%--------------------------------------------------------------------
%% @doc puts the chars back into a ticks number
%% This is how the arduino accepts the ticks.
%% @spec chars_to_ticks(High::char(), Low::char()) -> Ticks:int().
%% @end
%%--------------------------------------------------------------------
chars_to_ticks(High, Low) ->
    (Low * 256)  + High.

%%--------------------------------------------------------------------
%% @doc
%% @spec inches_to_metres(Inches::float()) -> Metres:float().
%% @end
%%--------------------------------------------------------------------
inches_to_metres(Inches) ->
    Inches * 0.0254.

%%--------------------------------------------------------------------
%% @doc the circumference of a roller in metres whose diamter in inches is DiamInches
%% @spec roller_circ(DiamInches:float()) -> Metres:float().
%% @end
%%--------------------------------------------------------------------
roller_circ(DiamInches) ->
    inches_to_metres(DiamInches) * math:pi().

%%--------------------------------------------------------------------
%% @doc How long is the race in metres (if the Ticks and Roller Circumerence are these)
%% @spec ticks_to_length(Ticks:int(), RollerCirc:float()) -> Metres:int().
%% @end
%%--------------------------------------------------------------------
ticks_to_length(Ticks, RollerCirc) ->
    Ticks * RollerCirc.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
