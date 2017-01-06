-module(game_server_center_table).

-behaviour(gen_server).

-export([start_link/0]).

-export([get_all/1,message_to_game_server/2]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("game_server_record.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_all(RequesterIp) ->
  Q = ets:tab2list(game_table),
  lists:filter(fun(
    #game_server{
      game_id = GameId,
      game_name = GameName,
      game_type = GameType,
      address = Address,
      game_server_status = Status,
      white_ip = WhiteIpList}) ->
        Condition1 = ( Status =:= 1 ),
        Condition2 = (Status =:= 2) andalso lists:member(RequesterIp,WhiteIpList),
        case Condition1 orelse Condition2 of
          false -> false;
          true -> {true, #{
            game_id => GameId,
            game_name => GameName,
            game_type => GameType,
            address => Address}}
        end
               end,
    Q).

message_to_game_server(GameId,Message) ->
  case ets:lookup(game_table, GameId) of
    [GameServer] ->
      ConnectionPid = GameServer#game_server.connection_pid,
      erlang:send(ConnectionPid, {to_game_server, Message});
    _ ->
      ignore
  end.

init([]) ->
  ets:new(game_table,[named_table,{keypos,#game_server.game_id},protected]),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({new_game_server_maps,GameServerMaps}, State) ->
  #{
    game_id := GameId,
    game_name := GameName,
    game_type := GameType,
    address := Address,
    game_server_status := GameServerStatus,
    white_ip := WhiteIp,
    connection_pid := ConnectionPid
  } = GameServerMaps,
  GameServerRecord = #game_server{
    game_id = GameId,
    game_name = GameName,
    game_type = GameType,
    address = Address,
    game_server_status = GameServerStatus,
    white_ip = WhiteIp,
    connection_pid = ConnectionPid
  },
  ets:insert(game_table, GameServerRecord),
  {noreply, State};

handle_info({game_server_disconnected, GameId}, State) ->
  ets:delete(game_table, GameId),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
