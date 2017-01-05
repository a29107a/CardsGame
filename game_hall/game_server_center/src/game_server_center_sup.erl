-module(game_server_center_sup).

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

  GameServerAgentConfig = {
    'game_server_center_config',
    {'config_server', start_link, [game_server_center_config, "config/game_server_center.config"]},
    Restart, Shutdown, Type, ['game_server_center_config']
  },
  GameServerCenterTable = {
    'game_server_center_table',
    {'game_server_center_table', start_link(), []},
    Restart, Shutdown, Type, ['game_server_center_table']
  },
  {ok, {SupFlags, [GameServerAgentConfig, GameServerCenterTable]}}.

