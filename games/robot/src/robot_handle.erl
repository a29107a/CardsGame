-module(robot_handle).

-include("login_pb.hrl").
-include("game_pb.hrl").

-export([handle/2]).

-define(SOCKET_OPTIONS,[{active,once},{keepalive, true},{mode, binary},{packet,4}]).

handle(Info, State) ->
  lager:info("~p try to handle info : ~p when state is : ~p", [?MODULE,Info,State]),
  handle2(Info,State).

handle2(init,State) ->
  {LoginIp,LoginPort} = config_server:get(robot_config_server,target_login_server_address),
  case gen_tcp:connect(LoginIp,LoginPort,?SOCKET_OPTIONS) of
    {ok,Socket} ->
      NewState = State#{to_login_server_socket => Socket},
      AccountNameUuid = lists:filter(fun(I) -> I =/= $- end, uuid:to_string(uuid:uuid4() ) ),
      ClLogin = #cl_login{platform_id = 0, parameters = AccountNameUuid},
      robot:send_request_to(to_login_server_socket, NewState,ClLogin),
      NewState;
    {error, Reason} ->
      lager:error( "Cannot connect to LoginIp: ~p, LoginPort: ~p with error reason: ~p",[LoginIp, LoginPort,Reason]),
      stop
  end;

handle2({tcp,ToLoginServerSocket, Data},#{to_login_server_socket := ToLoginServerSocket} = State) ->
  Decoder = maps:get(decoder,State),
  Message = Decoder:decode(Data),
  lager:info( "Received Message: ~p when State: ~p", [Message,State]),
  RobotMessageHandleResult = robot_message:handle(Message, State),
  inet:setopts(ToLoginServerSocket, [{active,once}]),
  RobotMessageHandleResult.

handle2(Info,State) ->
  lager:error( "Unhandled Info: ~p when State is ~p",[Info,State]).
