-module(login_server_connection_terminate).

-compile(export_all).

terminate(Reason,Connection) ->
  lager:info("Connection: ~p terminated with Reason: ~p" ,[ Connection, Reason]).
