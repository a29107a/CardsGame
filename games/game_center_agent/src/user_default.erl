-module(user_default).

-compile(export_all).

start_game_center_agent() ->
  ok = erlang:element(1, application:ensure_all_started(lager)),
  ok = erlang:element(1, application:ensure_all_started(game_center_agent)),
  lager:info( "game center agent started at node: ~p", [ erlang:node() ]).
