-module(login_server_utils).

-compile(export_all).

account_id_to_login_process_name(AccountId) ->
  AccountIdList = erlang:integer_to_list(AccountId),
  ProcessName = erlang:list_to_atom("login_server_connection_" ++ AccountIdList),
  ProcessName.
