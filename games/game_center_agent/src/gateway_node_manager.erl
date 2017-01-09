-module(gateway_node_manager).
-behaviour(gen_server).
-export([start_link/0]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #{}}.

handle_call({register_a_gateway_node,Node, NodeInfo},_From, State) ->
  GatewayNodeList = maps:get(gateway_node_list, State, []),
  NewGatewayNodeList =
  case lists:keymember(Node,1,GatewayNodeList) of
    true ->
      lists:keystore(Node,1, GatewayNodeList,{Node, NodeInfo});
    false ->
      erlang:monitor_node(Node, true),
      [Node | GatewayNodeList]
  end,
  NewState = maps:put(gateway_node_list, NewGatewayNodeList, State),
  {reply, ok, NewState};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({nodedown, Node}, State) ->
  NewState = maps:update_with(gateway_node_list,
    fun(OldList) -> lists:keydelete(Node, 1, OldList) end,
      State),
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

