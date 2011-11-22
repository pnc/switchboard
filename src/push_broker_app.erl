%% @author Mochi Media <dev@mochimedia.com>
%% @copyright push_broker Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the push_broker application.

-module(push_broker_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for push_broker.
start(_Type, _StartArgs) ->
    push_broker_deps:ensure(),
    push_broker_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for push_broker.
stop(_State) ->
    ok.
