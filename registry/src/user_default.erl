-module(user_default).

-compile(export_all).

start_registry_server() ->
  ok = erlang:element(1, application:ensure_all_started(registry)).
