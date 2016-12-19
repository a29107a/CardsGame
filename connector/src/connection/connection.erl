-module(connection).
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
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(Ref, Socket, Transport, Opts) ->
  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) ->
  {ok, not_used}.

init(Ref, Socket, Transport, _Opts = []) ->
  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  {ok, {Ip,Port}} = inet:peername(Socket),
  ThisMilliSeconds = erlang:system_time(milli_seconds),
  IpStringBinary = unicode:characters_to_binary(inet:ntoa(Ip)),
  State = #{
    socket => Socket,
    transport => Transport,
    ip => IpStringBinary,
    port => Port,
    last_tcp_passive_milli_seconds => ThisMilliSeconds,
    last_heart_beat => ThisMilliSeconds},
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

handle_info({tcp_passive, Socket},#{socket := Socket, last_tcp_passive_milli_seconds := LastTcpPassiveMilliSeconds} = State)->
  NowMilliSeconds = erlang:system_time(milli_seconds),
%%  AveragePacketsPerMilliSeconds = ?ACTIVE_NUM / ( NowMilliSeconds - LastTcpPassiveMilliSeconds ) ,
  ReceivedPacketsBetweenMilliSeconds = NowMilliSeconds - LastTcpPassiveMilliSeconds,
  case ReceivedPacketsBetweenMilliSeconds < ?CLOSE_SOCKET_PACKETS_THRESHOLD of
    true->
      % client may using accelerator, stop this client.
      {stop,{error,send_too_fast,ReceivedPacketsBetweenMilliSeconds},State};
    false->
      inet:setopts(Socket, [{active,?ACTIVE_NUM}]),
      {noreply,maps:put(last_tcp_passive_milli_seconds,NowMilliSeconds,State)}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

