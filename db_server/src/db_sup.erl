-module(db_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {'db_server', {'db_server', start_link, []}, Restart, Shutdown, Type, ['db_server']},

  {ok, {SupFlags, [AChild]}}.
