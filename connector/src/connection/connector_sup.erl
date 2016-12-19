-module(connector_sup).
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

  ConnectionSup = {connection_sup,{connection,start_link,[]},permanent,5000, supervisor,[connection_sup]},

  RanchSpec = {ranch_sup, {ranch_sup, start_link, []}, permanent, 5000, supervisor, [ranch_sup]},
  {ok,ConfigurationList} = file:consult("config/connector.config"),
  ListenPort = proplists:get_value(listen_port,ConfigurationList,29999),
  AcceptorsNum = 64,
  ListenerOptions = [{port, ListenPort},{connection_type, supervisor},{keepalive, true},{max_connections,10240*15}],
  ListenerSpec = ranch:child_spec(connector, AcceptorsNum, ranch_tcp, ListenerOptions, connection_sup, []),


  {ok, {SupFlags, [ConnectionSup,RanchSpec,ListenerSpec]}}.

