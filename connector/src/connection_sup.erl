-module(connection_sup).
-behaviour(supervisor).
-behaviour(ranch_protocol).

-export([start_link/0]).
-export([start_link/1]).
-export([start_link/4]).


-export([init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

start_link(ConnectionSupName) ->
  supervisor:start_link({local,ConnectionSupName}, ?MODULE, []).

start_link(Ref, Socket, Transport, CustomOpts) ->
  SupervisorPid = maps:get(ref_connection_sup,CustomOpts),
  case supervisor:start_child(SupervisorPid, [Ref, Socket, Transport, CustomOpts]) of
    {ok, ConnectionPid} ->
      {ok, SupervisorPid, ConnectionPid};
    StartChildError ->
      {error,StartChildError}
  end.

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = temporary,
  Shutdown = timer:seconds(60),
  Type = worker,
  ConnectionChild = {connection_protocol,
    {connection_protocol, start_link, []},
    Restart,
    Shutdown,
    Type,
    [connection_protocol]
  },
  {ok, {SupFlags, [ConnectionChild]}}.
