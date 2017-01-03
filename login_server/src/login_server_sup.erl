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
    'login_server_config',
    {'login_server_config', start_link, []},
    Restart, Shutdown, Type,
    ['login_server_config']
  },
  RegistryServerAgent = {
    'login_server_registry_agent',
    {'login_server_registry_agent', start_link, []},
    Restart, Shutdown, Type,
    ['login_server_registry_agent']
  },
  {ok, {SupFlags, [LoginConfigServer,RegistryServerAgent]}}.
