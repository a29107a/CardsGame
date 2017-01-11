-module(robot).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([start_robot/0,start_robot/1]).
-export([send_request_to/3]).

start_robot() ->
  start_robot([]).

start_robot(Parameters) when erlang:is_list(Parameters)->
  supervisor:start_child(robots_sup,Parameters);
start_robot(Parameters) ->
  supervisor:start_child(robots_sup,[Parameters]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  erlang:send(erlang:self(), init),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  try
    case robot_handle:handle(Info,State) of
      NewState when erlang:is_map(NewState) ->
        {noreply, NewState};
      stop ->
        {stop, normal, State};
      _ ->
        {noreply, State}
    end
  catch
    ErrorType:ErrorReason ->
      lager:error("robot, ErrorType: ~p,ErrorReason: ~p", [ErrorType, ErrorReason])
  end.

terminate(Reason, State) ->
  lager:info( "Robot Terminated with Reason: ~p, State: ~p",[Reason, State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send_request_to(Where,Connection,Reply) when erlang:is_binary(Reply) ->
  #{Where := Socket, transport := Transport} = Connection,
  Transport:send(Socket, Reply);
send_request_to(Where,Connection,Reply) when erlang:is_tuple(Reply) ->
  Encoder = maps:get(encoder,Connection),
  BinaryReply = Encoder:encode(Reply),
  send_request_to(Where,Connection, BinaryReply).
