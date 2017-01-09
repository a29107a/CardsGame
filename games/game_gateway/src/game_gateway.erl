-module(game_gateway).

-export([start_game_gateway/0]).

start_game_gateway() ->
  GameCenterAgentNode = config_server:get(game_gateway_config_server,game_center_agent_node),
  case net_adm:ping(GameCenterAgentNode) of
    pong ->
      next;
    pang ->
      lager:critical( "Cannot find game_agent_Node: ~p", [ GameCenterAgentNode ])
  end,
  GatewayListenPort = config_server:get(game_gateway_config_server,game_gateway_listen_port),
  AcceptorNum = utilities:get_cpu_num() * 8,
  CustomOpts = #{
    encoder => pb_codec,
    decoder => pb_codec,
    handler => game_gateway_handle,
    terminate => game_gateway_terminate
  },
  connection:start_connection(?MODULE,GatewayListenPort,AcceptorNum,CustomOpts).
