-module(connection).
-export([start_connection/4]).
-export([connector_child_ref/4]).

start_connection(RefName,ListenPort, AcceptorNum,Handler) ->
  supervisor:start_child(connector_sup,connector_child_ref(RefName,ListenPort,AcceptorNum,Handler)).

connector_child_ref(RefName,ListenPort, AcceptorNum,Handler) ->
  ApplicationChildSpec ={{connector,RefName},
    {connector_sup,start_link,[RefName,ListenPort, AcceptorNum,Handler]},
    permanent,
    5000,
    supervisor,
    [connector_sup]},
  ApplicationChildSpec.
