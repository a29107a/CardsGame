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

create_account(ParameterMaps) when erlang:is_map(ParameterMaps) ->
  PlatformId = maps:get(platform_id, ParameterMaps),
  AccountName = maps:get(account_name,ParameterMaps),
  AccountId = maps:get(account_id, ParameterMaps),
  AccountNameBinary = unicode:characters_to_binary(AccountName),
  true = erlang:is_integer(AccountId),
  true = erlang:is_integer(PlatformId),
  BsonMap = #{
    account_name => AccountNameBinary,
    platform_id => PlatformId,
    account_id => AccountId
  },
  {{true,ResultMap},Bson} = mongo_api:insert(login, <<"account">>,BsonMap),
  lager:info( "ResultMap: ~p", [ ResultMap ]),
  case maps:find(<<"n">>, ResultMap) of
    {ok,1} ->
      {true, Bson};
    _ ->
      false
  end.
