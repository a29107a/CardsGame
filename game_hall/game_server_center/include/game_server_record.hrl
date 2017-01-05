-ifndef(GAME_SERVER_RECORD).
-define(GAME_SERVER_RECORD,1).

-record(game_server, {
  game_id,
  game_name,
  game_type,
  address,
  game_server_status
}).

-endif.
