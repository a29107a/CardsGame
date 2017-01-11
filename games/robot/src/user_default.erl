-module(user_default).

-compile(export_all).

start_robot() ->
  ok = erlang:element(1, application:ensure_all_started(lager)),
  ok = erlang:element(1, application:ensure_all_started(robot)),
  lager:info( "robot app stared at node: ~p", [ erlang:node() ]).
