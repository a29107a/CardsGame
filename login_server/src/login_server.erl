-module(login_server).
-export([start/0]).

start() ->
  {ok,ConfigList} = file:consult("config/login_server.config"),
  ListenPort = proplists:get_value(login_server_listen_port, ConfigList, 30000),
  AcceptorNum = utilities:get_cpu_num() * 8,
  CustomOpts = #{encoder => pb_codec, decoder => pb_codec, handler => login_server_handle},
  connection:start_connection(?MODULE,ListenPort, AcceptorNum,CustomOpts).
