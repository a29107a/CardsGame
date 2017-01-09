-module(user_default).
-compile(export_all).

start_game_gateway() ->
  ok = erlang:element(1, application:ensure_all_started(game_gateway)),
  game_gateway:start_game_gateway(),
  GatewayAgentNode = config_server:get(game_gateway_config_server,game_center_agent_node),
  GatewayLisenAddress = config_server:get(game_gateway_config_server,game_gateway_listen_address),
  GatewayListenPort = config_server:get(game_gateway_config_server,game_gateway_listen_port),
  ok = gen_server:call({gateway_node_manager,GatewayAgentNode},
    {register_a_gateway_node,erlang:node(),{GatewayLisenAddress, GatewayListenPort}},
    timer:seconds(60)),
  lager:info( "game_gateway started at node:~p", [ erlang:node() ]).
