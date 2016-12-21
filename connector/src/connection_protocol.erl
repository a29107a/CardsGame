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
  State = maps:merge(#{
    socket => Socket,
    transport => Transport,
    ip => IpStringBinary,
    port => Port,
    last_tcp_passive_milli_seconds => ThisMilliSeconds,
    last_heart_beat => ThisMilliSeconds}, Opts),
  erlang:send(erlang:self(),initialize_socket_parameters),
  gen_server:enter_loop(?MODULE, [],State, timer:seconds(10)).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(initialize_socket_parameters, State) ->
  Socket = maps:get(socket,State),
  inet:setopts(Socket, [{active, ?ACTIVE_NUM},{packet, 4},{keepalive, true},binary]),
  {noreply,State};

handle_info({tcp, Socket, BinarySocketData}, #{socket := Socket} = Connection) ->
  try
    Decoder = maps:get(decoder,Connection,decode),
    Handler = maps:get(handler,Connection,handle),
    Message = Decoder:decode(BinarySocketData),
    case Handler:handle(Message,Connection) of
      NewConnection when erlang:is_map(NewConnection) ->
        {noreply, NewConnection};
      {reply,Reply} when erlang:is_binary(Reply) orelse erlang:is_tuple(Reply) ->
        send_reply(Connection, Reply),
        {noreply, Connection};
      {reply,Reply,NewConnection} when (erlang:is_binary(Reply) orelse erlang:is_tuple(Reply)) andalso erlang:is_map(NewConnection) ->
        send_reply(NewConnection,Reply),
        {noreply, NewConnection};
      _ ->
        {noreply,Connection}
    end
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

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send_reply(Connection,Reply) when erlang:is_binary(Reply) ->
  #{socket := Socket, transport := Transport} = Connection,
  Transport:send(Socket, Reply);
send_reply(Connection,Reply) when erlang:is_tuple(Reply) ->
  #{encoder := Encoder} = Connection,
  BinaryReply = Encoder:encode(Reply),
  send_reply(Connection, BinaryReply).
