%% @author author <russell@ossme.net>
%% @copyright 2010 Russell Brown.

%% @doc starts the roller app and collaborators.

-module(roller).
-author('Russell Brown <russell@ossme.net>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the roller server.
start() ->
    roller_deps:ensure(),
    ensure_started(tempile),
    application:start(roller).

%% @spec stop() -> ok
%% @doc Stop the roller server.
stop() ->
    Res = application:stop(roller),
    application:stop(tempile),
    Res.
