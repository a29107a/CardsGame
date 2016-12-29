-module(user_default).

-compile(export_all).

start_login_server() ->
  application:ensure_all_started(login_server),
  login_server:start().
