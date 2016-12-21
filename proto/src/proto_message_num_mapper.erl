-module(proto_message_num_mapper).

-export([get/1]).

-define(L(Message), {login, Message}).

get(cl_login) -> ?L(1001);
get(1001) -> ?L(cl_login);

get(lc_login_result) -> ?L(1002);
get(1002) -> ?L(lc_login_result);

get(cl_fetch_game_server_list) -> ?L(1003);
get(1003) -> ?L(cl_fetch_game_server_list);

get(lc_fetched_game_server_list) -> ?L(1004);
get(1004) -> ?L(lc_fetched_game_server_list);

get(cl_select_game_server) -> ?L(1005);
get(1005) -> ?L(cl_select_game_server);

get(1006) -> ?L(lc_select_game_server_result);
get(lc_select_game_server_result) -> ?L(1006);

get(_) -> not_supported_protocol.
