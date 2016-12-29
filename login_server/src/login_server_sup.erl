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
  AChild = {'login_config_server',
    {'login_config_server', start_link, []},
    Restart, Shutdown, Type,
    ['login_config_server']},
  {ok, {SupFlags, [AChild]}}.
