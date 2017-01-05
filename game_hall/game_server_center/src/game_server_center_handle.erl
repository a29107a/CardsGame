-module(game_server_center_handle).

-include("game_server_agent.hrl").

-export([handle/2]).

handle(Message,Connection) ->
  lager:info("Module: ~p handle message : ~p when Connection state is: ~p", [?MODULE,Message,Connection]),
  handle2(Message,Connection).

handle2(custom_initialization,_) ->
  maps:new();

handle2(Message, Connection) when erlang:is_record(Message,gl_register_game_server) ->
  #gl_register_game_server{game_id=GameId,
    game_name = GameName,
    game_type = GameType,
    address = Address,
    game_server_status = GameServerStatus} = Message,
  GameServerMaps = #{
    game_id => GameId,
    game_name => unicode:characters_to_binary(GameName),
    game_type => GameType,
    address => Address,
    game_server_status => GameServerStatus
  },
  erlang:send(game_server_agent_table,{new_game_server_maps,GameServerMaps}),
  maps:merge(Connection,GameServerMaps).
