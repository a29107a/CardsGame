-module(user_default).

-compile(export_all).

start_game_server_agent_server() ->
  ok = erlang:element(1, application:ensure_all_started(lager)),
  ok = erlang:element(1, application:ensure_all_started(game_server_center)),
  game_server_center:start(),
  register_to_registry_node(),
  lager:info( "game server center at node: ~p started.", [ erlang:node() ]).

register_to_registry_node() ->
  {ok, ConfigList} = file:consult("config/game_server_center.config"),
  RegistryNodeList = proplists:get_value(registry_node_list,ConfigList, []),
  RegisterInfo = {register_game_server_center_node,erlang:node()},
  register_to_registry_node:register(RegistryNodeList,registered_game_server_center,RegisterInfo).
