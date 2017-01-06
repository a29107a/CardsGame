-module(login_server_registry_agent).
-behaviour(gen_server).

-export([get_one_db_node/0]).
-export([get_one_game_center_node/0]).
-export([get_one_service/0]).
-export([start_link/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

get_one_db_node() ->
  case gen_server:call(?MODULE, get_login_db_nodes,timer:seconds(30)) of
    [] ->
      erlang:exit(erlang:self(),normal);
    Lists when erlang:length(Lists) > 0 ->
      lists:nth(rand:uniform(erlang:length(Lists)), Lists);
    _ ->
      erlang:exit(erlang:self(), normal)
  end.

get_one_game_center_node() ->
  case gen_server:call(?MODULE, get_game_center_nodes,timer:seconds(30)) of
    [] ->
      erlang:exit(erlang:self(),normal);
    Lists when erlang:length(Lists) > 0 ->
      lists:nth(rand:uniform(erlang:length(Lists)), Lists);
    _ ->
      erlang:exit(erlang:self(), normal)
  end.

get_one_service() ->
  case gen_server:call(?MODULE, get_game_center_nodes,timer:seconds(30)) of
    OneMap when erlang:is_map(OneMap) ->
      #{login_db_nodes := LoginDbNodes,game_center_nodes := GameCenterNodes} = OneMap,
      OneLoginDbNode = utilities:list_random_one(LoginDbNodes),
      OneGameCenterNode = utilities:list_random_one(GameCenterNodes),
      #{one_login_db_node => OneLoginDbNode, one_game_center_node => OneGameCenterNode};
    _ ->
      maps:new()
  end.


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  ets:new(db_nodes,[named_table,{keypos, 2}]),
  reload(),
  erlang:send_after(timer:seconds(2),erlang:self(),refresh),
  {ok, #{}}.

handle_call(get_login_db_nodes,_From,State) ->
  LoginDbNodes = maps:get(login_db_nodes,State, []),
  {reply, LoginDbNodes, State};

handle_call(get_game_center_nodes, _From, State) ->
  GameCenterNodes =maps:get(game_center_nodes, State, []),
  {reply, GameCenterNodes, State};

handle_call(get_one_service, _From, State) ->
  {reply, State, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(refresh,State) ->
  erlang:send_after(timer:hours(2),erlang:self(),refresh),
  reload(),
  {noreply, State};

handle_info(manual_refersh, State) ->
  reload(),
  {noreply, State};

handle_info({to_agent, login_db_nodes, LoginDbNodeList}, State) ->
  NewState =
    maps:update_with(login_db_nodes,
      fun(OldList) -> lists:usort(LoginDbNodeList ++ OldList) end,
      LoginDbNodeList,
      State),
  {noreply, NewState};

handle_info({to_agent, game_center_nodes, GameServerCenterNodeList}, State) ->
  NewState =
    maps:update_with(game_center_nodes,
      fun(OldList) -> lists:usort(GameServerCenterNodeList ++ OldList) end,
      GameServerCenterNodeList,
      State),
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

reload() ->
  {ok, ConfigList} = file:consult("config/login_server.config"),
  RegistryNodes = proplists:get_value(registry_node_list,ConfigList),
  lists:foreach(fun(RegistryNode) ->
    SelfPid = erlang:self(),
    erlang:send({registered_db_server, RegistryNode},{request_login_db_nodes,SelfPid}),
    erlang:send({registered_game_server_center,RegistryNode},{request_game_server_center_nodes,SelfPid})
                end,
    RegistryNodes).
