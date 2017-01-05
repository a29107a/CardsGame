-module(user_default).

-compile(export_all).

start_login_server() ->
  ok = erlang:element(1, application:ensure_all_started(lager)),
  ok = erlang:element(1, application:ensure_all_started(login_server)),
  login_server:start(),
  lager:info( "Login server at node: ~p started.", [ erlang:node() ]).
