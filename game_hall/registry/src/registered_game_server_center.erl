-module(registered_game_server_center).
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
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({register_game_server_center_node,Node}, State) ->
  GameServerCenterNodeList = maps:get(game_server_center_node_list,State, []),
  GameServerCenterNodeList1 =
  case lists:member(Node, GameServerCenterNodeList) of
    true ->
      GameServerCenterNodeList;
    false ->
      erlang:monitor_node(Node, true),
      [Node | GameServerCenterNodeList]
  end,
  NewState = maps:put(game_server_center_node_list, GameServerCenterNodeList1, State),
  {noreply, NewState};

handle_info({request_game_server_center_nodes, Requester}, State) ->
  GameServerCenterNodeList = maps:get(game_server_center_node_list,State, []),
  erlang:send(Requester,{to_agent, game_center_nodes, GameServerCenterNodeList}),
  {noreply, State};

handle_info({nodedown, Node}, State) ->
  NewState = maps:update_with(game_server_center_node_list,
    fun(OldList) -> lists:usort(lists:delete(Node,OldList)) end,
    State),
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
