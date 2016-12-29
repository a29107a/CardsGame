-module(login_config_server).
-behaviour(gen_server).

-export([reload/0, get/1]).
-export([start_link/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

reload() ->
  erlang:send(?MODULE,reload_config).

get(Key) ->
  [{Key,Value}] = ets:lookup(login_config, Key),
  Value.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(login_config,[named_table,protected]),
  load_config(),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(reload_config, State) ->
  load_config(),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

load_config() ->
  {ok, Terms} = file:consult("config/login_server.config"),
  ets:insert(login_config, Terms).
