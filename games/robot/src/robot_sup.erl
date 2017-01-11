-module(robot_sup).
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

  RobotConfigServer = {'robot_config_server',
    {'config_server', start_link, [robot_config_server, "config/robot.config"]},
    Restart, Shutdown, Type, ['robot_config_server']},
  RobotsSupervisor = { 'robots_sup',
    {'robots_sup',start_link, []},
    Restart, Shutdown, supervisor, ['robots_sup']
    },

  {ok, {SupFlags, [RobotConfigServer,RobotsSupervisor]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
