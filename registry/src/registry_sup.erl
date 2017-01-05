-module(registry_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10000,
  MaxSecondsBetweenRestarts = 36000,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 20000,
  Type = worker,

  RegisteredDbServer = {
    'registered_db_server',
    {'registered_db_server', start_link, []},
    Restart, Shutdown, Type, ['registered_db_server']},
  RegisteredGameServerCenter = {
    registered_game_server_center,
    {registered_game_server_center,start_link, []},
    Restart, Shutdown, Type, [registered_game_server_center]
    },
  {ok, {SupFlags, [RegisteredDbServer, RegisteredGameServerCenter]}}.
