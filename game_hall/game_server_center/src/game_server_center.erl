-module(game_server_center).

-export([start/0]).


start() ->
  ListenPort = config_server:get(game_server_center_config,game_server_center_listen_port),
  AcceptorNum = 2,
  CustomOpts = #{
    encoder => pb_codec,
    decoder => pb_codec,
    handler => game_server_center_handle,
    terminate => game_server_center_connection_terminate},
  connection:start_connection(?MODULE,ListenPort, AcceptorNum,CustomOpts),
  lager:info( "game server agent started with LisenPort: ~p, AcceptorNum: ~p, CustomOpts: ~p",[ListenPort,AcceptorNum,CustomOpts]).
