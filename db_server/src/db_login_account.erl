-module(db_login_account).

-compile(export_all).

get_account(ParameterMaps) when erlang:is_map(ParameterMaps) ->
  PlatformId = maps:get(platform_id, ParameterMaps),
  AccountName = maps:get(account_name,ParameterMaps),
  AccountNameBinary = unicode:characters_to_binary(AccountName),
  Selector = {account_name,AccountNameBinary,platform_id,PlatformId},
  OneMap = mongo_api:find_one(login, <<"account">>,Selector,{}, 0),
  case erlang:is_map(OneMap) andalso erlang:map_size(OneMap) > 0 of
    true ->
      OneMap;
    false ->
      {}
  end.

