-module(register_to_registry_node).

-compile(export_all).

register(RegistryNodes,ProcessName,RegistryInfo) when erlang:is_list(RegistryNodes) ->
  lists:foreach(fun(RegistryNode) -> register(RegistryNode,ProcessName,RegistryInfo) end, RegistryNodes);
register(RegistryNode,ProcessName,RegistryInfo) ->
  case os:getenv("DEBUG") of
    false ->
      case net_adm:ping(RegistryNode) of
        pong ->
          erlang:send({ProcessName,RegistryNode},RegistryInfo);
        pang ->
          lager:warning("Cannot ping to node: ~p", [RegistryNode])
      end;
    _ ->
      ignore
  end.
