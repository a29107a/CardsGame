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

handle2(Message, Connection) when erlang:is_record(Message, cl_login)->
  #cl_login{platform_id = PlatformId, parameters = QuickLoginDeviceString} = Message,
  DbNode = maps:put(db_node,Connection),
  case PlatformId of
    0 ->
      QuerySelector = #{platform_id => PlatformId,account_name => QuickLoginDeviceString},
      case rpc:call(DbNode, db_login_account, get_account, [QuerySelector]) of
        OneMap when erlang:is_map(OneMap) ->
          AccountId = maps:get(account_id, OneMap),
          AccountInfo = #account_info{account_id = AccountId},
          {reply, #lc_login_result{error_code = 0, account_id = AccountId, account_info = AccountInfo}};
        {} ->
          CreateMaps = #{platform_id => PlatformId,account_name => QuickLoginDeviceString},
          case rpc:call(DbNode,db_login_account,create_account, [CreateMaps]) of
            false ->
              {reply, #lc_login_result{error_code = 1}};
            {true,OneMap} ->
              AccountId = maps:get(account_id, OneMap),
              AccountInfo = #account_info{account_id = AccountId},
              {reply, #lc_login_result{error_code = 0, account_id = AccountId, account_info = AccountInfo}}
          end
      end;
    _ ->
      {reply, #lc_login_result{error_code=2}}
  end.
