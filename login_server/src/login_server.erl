-module(login_server).
-export([start/0]).

start() ->
  ListenPort = login_server_config:get(listen_port),
  AcceptorNum = utilities:get_cpu_num() * 8,
  CustomOpts = #{
    encoder => pb_codec,
    decoder => pb_codec,
    handler => login_server_handle,
    terminate => login_server_connection_terminate},
  connection:start_connection(?MODULE,ListenPort, AcceptorNum,CustomOpts),
  lager:info( "login server started with LisenPort: ~p, AcceptorNum: ~p, CustomOpts: ~p",[ListenPort,AcceptorNum,CustomOpts]).
