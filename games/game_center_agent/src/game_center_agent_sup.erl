-module(game_center_agent_sup).
-behaviour(supervisor).
-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10000,
  MaxSecondsBetweenRestarts = 36000,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 20000,
  Type = worker,

  GameCenterAgentConfigServer = {'game_center_agent_config_server',
    {'config_server', start_link, [global,game_center_agent_config_server,"config/game_center_agent.config"]},
    Restart, Shutdown, Type, ['game_center_agent_config_server']},
  GameCenterAgentConnection = {'game_center_agent_connection',
    {'game_center_agent_connection', start_link, []},
    Restart, Shutdown, Type, ['game_center_agent_connection']},
  GameNodeManager = {game_node_manager,
    {game_node_manager,start_link,[]},
    Restart, Shutdown, Type, [game_node_manager]},
  GatewayNodeManager = {gateway_node_manager,
    {gateway_node_manager, start_link, []},
    Restart, Shutdown, Type, [gateway_node_manager]
    },
  GameCenterAgentRegister = { game_center_agent_register,
    {game_center_agent_register, start_link, []},
    Restart, Shutdown, Type, [game_center_agent_register]
  },
  GameSessionServer = {game_session_server,
    {game_session_server,start_link, []},
    Restart, Shutdown, Type, [game_session_server]
    },
  Children = [
    GameCenterAgentConfigServer,
    GameCenterAgentConnection,
    GameNodeManager,
    GatewayNodeManager,
    GameCenterAgentRegister,
    GameSessionServer
  ],
  {ok, {SupFlags, Children}}.

