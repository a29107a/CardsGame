-module(connection_sup).
-behaviour(supervisor).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([start_link/0]).

-export([init/1]).

start_link(Ref, Socket, Transport, Opts) ->
  SupervisorPid =
    case erlang:whereis(?MODULE) of
      undefined ->
        Ret = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
        case erlang:element(1, Ret) of
          ok -> erlang:element(2, Ret);
          already_started -> erlang:element(2, Ret)
        end;
      Pid ->
        Pid
    end,
  {ok, ConnectionPid} = supervisor:start_child(SupervisorPid, [Ref, Socket, Transport, Opts]),
  {ok, SupervisorPid, ConnectionPid}.


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = temporary,
  Shutdown = timer:seconds(60),
  Type = worker,
  ConnectionChild = {'connection', {'connection', start_link, []}, Restart, Shutdown, Type, ['connection']},
  {ok, {SupFlags, [ConnectionChild]}}.
