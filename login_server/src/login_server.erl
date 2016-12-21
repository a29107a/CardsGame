-module(login_server).
-export([start/0]).

start() ->
  {ok,ConfigList} = file:consult("config/login_server.config"),
  ListenPort = proplists:get_value(login_server_listen_port, ConfigList, 30000),
  connection:start_connection(?MODULE,ListenPort,)
