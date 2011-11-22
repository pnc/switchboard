%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc push_broker.

-module(push_broker).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the push_broker server.
start() ->
    push_broker_deps:ensure(),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    application:start(push_broker).


%% @spec stop() -> ok
%% @doc Stop the push_broker server.
stop() ->
    application:stop(push_broker).
