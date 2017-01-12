-module(login_server_handle).

-include("login_pb.hrl").

-export([handle/2]).

handle(Message,Connection) ->
  lager:info("Module:~p  handling message:~p when Connection state is: ~p", [?MODULE,Message,Connection]),
  try
    handle2(Message,Connection)
  catch
    ErrorType:ErrorReason ->
      lager:error("~p catch error, ErrorType: ~p, ErrorReason: ~p, stacktrace: ~p",
        [?MODULE, ErrorType, ErrorReason, erlang:get_stacktrace()])
  end.

handle2(custom_initialization,Connection) ->
  OneService= login_server_registry_agent:get_one_service(),
  #{one_login_db_node := OneDbNode, one_game_center_node := OneGameCenterNode} = OneService,
  NewConnection = Connection#{db_node => OneDbNode, game_center_node => OneGameCenterNode},
  NewConnection;

handle2(Message, Connection) when erlang:is_record(Message, cl_login)->
  #cl_login{platform_id = PlatformId, parameters = QuickLoginDeviceString} = Message,
  DbNode = maps:get(db_node,Connection),
  case PlatformId of
    0 ->
      QuerySelector = #{platform_id => PlatformId,account_name => QuickLoginDeviceString},
      case rpc:call(DbNode, db_login_account, get_account, [QuerySelector]) of
        OneMap when erlang:is_map(OneMap) ->
          AccountId = maps:get(account_id, OneMap),
          AccountInfo = #account_info{account_id = AccountId},
          AccountProcessName = login_server_utils:account_id_to_login_process_name(AccountId),
          case global:register_name(AccountProcessName, erlang:self()) of
            yes ->
              {reply,
                #lc_login_result{error_code = 0, account_id = AccountId, account_info = AccountInfo},
                  maps:merge(Connection,OneMap)};
            no ->
              {stop, #lc_login_result{error_code = 1}}
          end;
        {} ->
          CreateMaps = #{
            account_id => login_server_uid:get(),
            platform_id => PlatformId,
            account_name => QuickLoginDeviceString},
          case rpc:call(DbNode,db_login_account,create_account, [CreateMaps]) of
            false ->
              {reply, #lc_login_result{error_code = 2}};
            {true,OneMap} ->
              AccountId = maps:get(account_id, OneMap),
              AccountInfo = #account_info{account_id = AccountId},
              AccountProcessName = login_server_utils:account_id_to_login_process_name(AccountId),
              case global:register_name(AccountProcessName, erlang:self()) of
                yes ->
                  {reply,
                    #lc_login_result{error_code = 0, account_id = AccountId, account_info = AccountInfo},
                    maps:merge(Connection, OneMap)
                    };
                no ->
                  {stop, #lc_login_result{error_code = 1}}
              end
          end
      end;
    _ ->
      {reply, #lc_login_result{error_code=3}}
  end;

handle2(Message, Connection) when erlang:is_record(Message, cl_fetch_game_server_list) ->
  #{game_center_node := GameCenterNode, ip := IpStringBinary } = Connection,
  case rpc:call(GameCenterNode,game_server_center_table,get_all, [ IpStringBinary ]) of
    List when erlang:is_list(List) ->
      GameServers = [
        #game_server_info{
          server_game_id = GameId,
          server_game_type = GameType,
          server_name = GameName
          } ||
        #{ game_id := GameId,
        game_name :=  GameName,
        game_type := GameType,
        address := _Address} <- List ],
      {reply,#lc_fetched_game_server_list{error_code = 0,game_servers = GameServers},Connection#{game_servers => List}};
    _ ->
      {stop, #lc_fetched_game_server_list{error_code = 1}}
  end.

%%handle2(Message,Connection) when erlang:is_record(Message, cl_select_game_server) ->
%%  #cl_select_game_server{game_id = GameId} = Message,
%%  GameServers = maps:get(game_servers,Connection, []),
%%  #{game_center_node := GameCenterNode} = Connection,
%%  case lists:filter(fun(#{game_id := GameId}) -> true;(_) -> false end,GameServers) of
%%    [GameServer] ->
%%      #{address := Address} = GameServer,
%%      {Ip, Port} = utilities:list_random_one(Address),
%%      Token = uuid:to_string(uuid:uuid4()),
%%%%      rpc:cast()%% ready to login.
%%      timer:sleep(timer:seconds(2)),
%%      {reply, #lc_select_game_server_result{error_code = 0,
%%        ip = Ip,
%%        port = Port,
%%        login_game_server_token = Token}};
%%    _ ->
%%      {reply, #lc_select_game_server_result{error_code = 1}}
%%  end.
