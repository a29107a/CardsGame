-module(registry_server_agent).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(db_nodes,[named_table,{keypos, 2}]),
  load_db_nodes(),
  erlang:send_after(timer:hours(2),erlang:self(),refresh_db_nodes),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(refresh_db_nodes,State) ->
  erlang:send_after(timer:hours(2),erlang:self(),refresh_db_nodes),
  load_db_nodes(),
  {noreply, State};

handle_info(manual_refersh, State) ->
  load_db_nodes(),
  {noreply, State};

handle_info({to_agent, login_db_nodes, LoginNodeList}, State) ->
  NewState = State#{login_db_nodes => LoginNodeList},
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

load_db_nodes() ->
  {ok, ConfigList} = file:consult("config/login_server.config"),
  RegistryNodes = proplists:get_value(registry_node_list,ConfigList),
  rpc:cast(RegistryNodes,registered_db_server, {request_login_db_nodes,erlang:self()}).
