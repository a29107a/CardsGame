-module(game_center_agent_connection).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {Ip,Port} = config_server:global_get(game_center_agent_config_server, game_center_address),
  case gen_tcp:connect(Ip,Port,[]) of
    {ok, Socket} ->
      {ok, #{socket => Socket}};
    {error, Reason} ->
      lager:error( "Cannot connector to ~p:~p with error reason: ~p", [Ip, Port,Reason]),
      {stop, Reason}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
