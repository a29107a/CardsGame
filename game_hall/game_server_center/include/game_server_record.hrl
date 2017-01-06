-ifndef(GAME_SERVER_RECORD).
-define(GAME_SERVER_RECORD,1).

-record(game_server, {
  game_id = 0,
  game_name = <<"">>,
  game_type = 0,
  address = [],
  game_server_status = 0, % 1 available to all,
                          % 2 white ip mode.
  white_ip = [],
  connection_pid = undefined
}).

-endif.
