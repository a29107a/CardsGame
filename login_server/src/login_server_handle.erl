-module(login_server_handle).

-include("login.hrl").

-export([handle/2]).

handle(Message, _Connection) when erlang:is_record(Message, cl_login)->
  #cl_login{how = How, parameters = _QuickLoginDeviceString} = Message,
  case How of
    0 ->
      ok;
    _ ->
      {reply, #lc_login_result{error_code=1}}
  end.
