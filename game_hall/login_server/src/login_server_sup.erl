-module(login_server_sup).
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
  Shutdown = 10000,
  Type = worker,
  LoginConfigServer = {
    'config_server',
    {'config_server', start_link, [login_server_config, "config/login_server.config"]},
    Restart, Shutdown, Type,
    ['config_server']
  },
  RegistryServerAgent = {
    'login_server_registry_agent',
    {'login_server_registry_agent', start_link, []},
    Restart, Shutdown, Type,
    ['login_server_registry_agent']
  },
  LoginServerUid = {
    login_server_uid,
    {login_server_uid,start_link, []},
    Restart,Shutdown,Type,
    [login_server_uid]
  },
  {ok, {SupFlags, [LoginConfigServer,RegistryServerAgent, LoginServerUid]}}.
