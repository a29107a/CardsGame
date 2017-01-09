-module(game_gateway_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  GameGatewayConfigServer = {game_gateway_config_server,
    {config_server,start_link,[game_gateway_config_server, "config/game_gateway.config"]},
    Restart, Shutdown, Type, [game_gateway_config_server]},

  {ok, {SupFlags, [GameGatewayConfigServer]}}.
