-module(user_default).

-compile(export_all).

start_db_server() ->
  ok = erlang:element(1, application:ensure_all_started(lager)),
  register_to_registry_node(),
  ok = erlang:element(1, application:ensure_all_started(db)),
  lager:info( "DbNode: ~p started. ", [erlang:node()]).

register_to_registry_node() ->
  {ok, ConfigList} = file:consult("config/db_server.config"),
  RegistryNodeList = proplists:get_value(registry_node_list,ConfigList, []),
  RegisterInfo = {register_login_db_node,erlang:node()},
  register_to_registry_node:register(RegistryNodeList,registered_db_server,RegisterInfo).
