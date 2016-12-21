-module(connector_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([start_link/4]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(RefName,ListenPort, AcceptorNum,Handler) ->
  RefSupName = erlang:list_to_atom( erlang:atom_to_list(RefName) ++ "_sup"),
  supervisor:start_link({local,RefSupName},?MODULE,{RefName,ListenPort,AcceptorNum,Handler}).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  RanchSpec = {ranch_sup, {ranch_sup, start_link, []}, permanent, 5000, supervisor, [ranch_sup]},
  {ok, {SupFlags, [RanchSpec]}};

init({RefName,ListenPort,AcceptorNum,CustomMapOpts}) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  RefConnectionSupName = erlang:list_to_atom(erlang:atom_to_list(RefName) ++ "_connection_sup"),
  ConnectionSup = {connection_sup,{connection_sup,start_link,[RefConnectionSupName]},permanent,5000, supervisor,[connection_sup]},
  ListenerOptions = [{port, ListenPort},{connection_type, supervisor},{keepalive, true},{max_connections,10240*16}],
  CustomOpts = maps:merge(CustomMapOpts,#{ref_connection_sup => RefConnectionSupName}),
  ListenerSpec = ranch:child_spec(RefName, AcceptorNum, ranch_tcp, ListenerOptions, connection_sup,CustomOpts),
  {ok, {SupFlags, [ListenerSpec, ConnectionSup]}}.





