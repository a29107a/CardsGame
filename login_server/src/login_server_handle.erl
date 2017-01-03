-module(login_server_handle).

-include("login.hrl").

-export([handle/2]).

handle(Message,Connection) ->
  lager:info("Module: ~p handle message : ~p when Connection state is: ~p", [?MODULE,Message,Connection]),
  handle2(Message,Connection).


handle2(custom_initialization,Connection) ->
  OneDbNode = login_server_registry_agent:get_one_db_node(),
  Connection1 = maps:put(db_node,OneDbNode, Connection),
  Connection1;

handle2(Message, _Connection) when erlang:is_record(Message, cl_login)->
  #cl_login{how = How, parameters = _QuickLoginDeviceString} = Message,
  case How of
    0 ->
      ok;
    _ ->
      {reply, #lc_login_result{error_code=1}}
  end.
