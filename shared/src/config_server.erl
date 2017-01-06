-module(config_server).
-behaviour(gen_server).

-export([reload/0, get/2,global_get/2]).
-export([start_link/0,start_link/2,start_link/3]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

reload() ->
  erlang:send(?MODULE,reload_config).

get(ConfigServerName,Key) ->
  Config = gen_server:call(ConfigServerName, get_state, timer:minutes(1)),
  maps:get(Key, Config).

global_get(ConfigServerName, Key) ->
  Config = gen_server:call({global, ConfigServerName},get_state,timer:minutes(1)),
  maps:get(Key, Config).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(ConfigServerName, ConfigFilePath) ->
  gen_server:start_link({local, ConfigServerName},?MODULE, ConfigFilePath, []).

start_link(GlobalOrLocal, ConfigServerName, ConfigFilePath) when GlobalOrLocal =:= global orelse GlobalOrLocal =:= local ->
  gen_server:start_link({GlobalOrLocal,ConfigServerName}, ?MODULE, ConfigFilePath, []);
start_link(_GlobalOrLocal, ConfigServerName, ConfigFilePath) ->
  start_link(ConfigServerName, ConfigFilePath).

init([]) ->
  {ok, maps:new()};
init(ConfigFilePath) ->
  Config = load_config(ConfigFilePath),
  {ok,Config#{config_file_path => ConfigFilePath}}.

handle_call(get_state,_From, State) ->
  {reply, State, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(reload_config, State) ->
  ConfigFilePath = maps:get(config_file_path,State),
  Config = load_config(ConfigFilePath),
  {noreply, maps:merge(State,Config) };

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

load_config(ConfigFilePath) ->
  {ok, Terms} = file:consult(ConfigFilePath),
  lists:foldl(fun({K,V}, Acc) ->
    maps:put(K,V, Acc)
              end,
    maps:new(),
    Terms).
