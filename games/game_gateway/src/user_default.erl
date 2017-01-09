-module(user_default).
-compile(export_all).

start_game_gateway() ->
  ok = erlang:element(1, application:ensure_all_started(lager)),
  ok = erlang:element(1, application:ensure_all_started(game_gateway)),
  game_gateway:start_game_gateway(),
  GatewayAgentNode = config_server:get(game_gateway_config_server,game_center_agent_node),
  GatewayLisenAddress = config_server:get(game_gateway_config_server,game_gateway_listen_address),
  GatewayListenPort = config_server:get(game_gateway_config_server,game_gateway_listen_port),
  case net_adm:ping(GatewayAgentNode) of
    pong ->
      next;
    pang ->
      lager:critical("Cannot ping to game server agent node: ~p", [ GatewayAgentNode ]),
      erlang:halt( 'Cannot ping to GatewayAgentNode')
  end,
  ok = global:sync(),
  RegisterInfo = {register_a_gateway_node,erlang:node(),{GatewayLisenAddress, GatewayListenPort}},
  case global:whereis_name(gateway_node_manager) of
    undefined ->
      lager:critical("Cannot find global process gateway_node_manager"),
      erlang:halt('Cannot find global process gateway_node_manager');
    Pid ->
      ok = gen_server:call(Pid, RegisterInfo, timer:seconds(60)),
      lager:info( "game_gateway started at node:~p", [ erlang:node() ])
  end.
