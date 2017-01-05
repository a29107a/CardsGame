-module(game_server_center_connection_terminate).

-compile(export_all).

terminate(Reason,Connection) ->
  lager:info( "~p terminated with Reason: ~p when Connection state is: ~p",[?MODULE,Reason,Connection]),
  GameId = maps:get(game_id, Connection, 0),
  erlang:send(game_server_center_table, {game_server_disconnected,GameId}).
