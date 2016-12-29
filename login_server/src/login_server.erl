-module(login_server).
-export([start/0]).

start() ->
  ListenPort = login_config_server:get(listen_port),
  AcceptorNum = utilities:get_cpu_num() * 8,
  CustomOpts = #{encoder => pb_codec, decoder => pb_codec, handler => login_server_handle},
  connection:start_connection(?MODULE,ListenPort, AcceptorNum,CustomOpts).
