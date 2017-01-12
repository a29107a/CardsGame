-module(connection_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([start_link/0,start_link/4]).

-export([
  init/1,
  init/4,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]
).

-define(ACTIVE_NUM, 30).
-define(CLOSE_SOCKET_PACKETS_THRESHOLD, 3000).
%%close this connection if received ACTIVE_NUM packets in CLOSE_SOCKET_PACKETS_THRESHOLD milliseconds

start_link() ->
  gen_server:start_link(?MODULE, [], []).

start_link(Ref, Socket, Transport, Opts) ->
  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) ->
  {ok, not_used}.

init(Ref, Socket, Transport, Opts) ->
  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  {ok, {Ip,Port}} = inet:peername(Socket),
  ThisMilliSeconds = erlang:system_time(milli_seconds),
  IpStringBinary = unicode:characters_to_binary(inet:ntoa(Ip)),
  DefaultSocketOptions = [{active, ?ACTIVE_NUM},{packet, 4},{keepalive, true},binary],
  ConnectionSocketOptions = maps:get(socket_options, Opts,DefaultSocketOptions),
  ok = inet:setopts(Socket,ConnectionSocketOptions),
  ScoketStateMap = #{
    socket => Socket,
    transport => Transport,
    ip => IpStringBinary,
    remote_port => Port,
    last_tcp_passive_milli_seconds => ThisMilliSeconds,
    last_heart_beat => ThisMilliSeconds},
  ConnectionMap = maps:merge(ScoketStateMap,Opts),
  erlang:send(erlang:self(),{use_handler,custom_initialization}),
  gen_server:enter_loop(?MODULE, [],ConnectionMap, timer:seconds(10)).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({use_handler,Message},Connection) ->
  try
      UserHandlerResult = handle(Message,Connection),
      lager:error( "UserHandlerResult: ~p", [ UserHandlerResult]),
      UserHandlerResult
  catch
      _ErrorType:_ErrorReason ->
        {noreply, Connection}
  end;

handle_info({tcp, Socket, BinarySocketData}, #{socket := Socket} = Connection) ->
  try
    Decoder = maps:get(decoder,Connection),
    Message = Decoder:decode(BinarySocketData),
    TcpDataHandleResult = handle(Message, Connection),
    lager:error( "TcpDataHandleResult: ~p", [ TcpDataHandleResult]),
    TcpDataHandleResult
    catch
      _ErrorType:_ErrorReason ->
        {noreply, Connection}
  end;

handle_info({tcp_passive, Socket},
  #{socket := Socket,last_tcp_passive_milli_seconds := LastTcpPassiveMilliSeconds} = State)->
  NowMilliSeconds = erlang:system_time(milli_seconds),
  ReceivedPacketsBetweenMilliSeconds = NowMilliSeconds - LastTcpPassiveMilliSeconds,
  case ReceivedPacketsBetweenMilliSeconds < ?CLOSE_SOCKET_PACKETS_THRESHOLD of
    true->
      % client may using accelerator, stop this client.
      {stop,{error,send_too_fast,ReceivedPacketsBetweenMilliSeconds},State};
    false->
      inet:setopts(Socket, [{active,?ACTIVE_NUM}]),
      {noreply,maps:put(last_tcp_passive_milli_seconds,NowMilliSeconds,State)}
  end;

handle_info({tcp_closed,_}, State) ->
  {stop, normal, State};

handle_info({tcp_error,_,Reason}, State) ->
  {stop, Reason, State};

handle_info(timeout, State) ->
  {stop, normal, State};

handle_info(Info, State) ->
  lager:error( "Module:~p, Unhandled info: ~p, State: ~p ~n", [?MODULE,Info, State]),
  {noreply, State}.

terminate(Reason,State) ->
  case maps:find(terminate, State) of
    {ok, TerminateHandler} when erlang:is_atom(TerminateHandler) ->
      TerminateHandler:terminate(Reason,State);
    error ->
      ignore
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle(Message, Connection) ->
  Handler = maps:get(handler,Connection),
  case Handler:handle(Message,Connection) of
    NewConnection when erlang:is_map(NewConnection) ->
      {noreply, NewConnection};
    {reply,Reply} when erlang:is_binary(Reply) orelse erlang:is_tuple(Reply) ->
      send_reply(Connection, Reply),
      {noreply, Connection};
    {reply,Reply,NewConnection} when (erlang:is_binary(Reply) orelse erlang:is_tuple(Reply)) andalso erlang:is_map(NewConnection) ->
      send_reply(NewConnection,Reply),
      {noreply, NewConnection};
    {stop,Reply} when erlang:is_binary(Reply) orelse erlang:is_tuple(Reply) ->
      send_reply(Connection, Reply),
      {stop,normal,Connection};
    {stop,Reply,NewConnection} when (erlang:is_binary(Reply) orelse erlang:is_tuple(Reply)) andalso erlang:is_map(NewConnection) ->
      send_reply(NewConnection,Reply),
      {stop, normal,NewConnection};
    stop ->
      {stop,normal, Connection};
    {stop,Reason} ->
      {stop,Reason, Connection}
  end.

send_reply(Connection,Reply) when erlang:is_binary(Reply) ->
  #{socket := Socket, transport := Transport} = Connection,
  Transport:send(Socket, Reply);
send_reply(Connection,Reply) when erlang:is_tuple(Reply) ->
  Encoder = maps:get(encoder,Connection),
  BinaryReply = Encoder:encode(Reply),
  send_reply(Connection, BinaryReply).
