-module(robot_message).

-include("login_pb.hrl").
-include("game_pb.hrl").

-export([handle/2]).

handle(Message,Robot) when erlang:is_record(Message, lc_login_result)->
  #lc_login_result{error_code = ErrorCode, account_id = AccountId, account_info = AccountInfo} = Message,
  case ErrorCode of
    0 ->
      #account_info{account_id = AccountId} = AccountInfo,
      robot:send_request_to(to_login_server_socket,Robot,#cl_fetch_game_server_list),
      maps:merge(Robot,#{account_id => AccountId});
    ErrorCode ->
      lager:error("Robot login error with error code: ~p", [ ErrorCode])
  end.

