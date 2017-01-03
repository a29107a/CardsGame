-module(registered_db_server).
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

handle_info({register_login_db_node,LoginDbNode},State) ->
  OldList = maps:get(login_db_node_list,State,[]),
  NewList =
    case lists:member(LoginDbNode,OldList) of
      true ->
        OldList;
      false ->
        erlang:monitor_node(LoginDbNode,true),
        [LoginDbNode|OldList]
    end,
  NewState = maps:put(login_db_node_list,NewList,State),
  {noreply, NewState};

handle_info({request_a_login_db_node,Requester},State) ->
  LoginNodeList = maps:get(login_db_node_list,State, []),
  case LoginNodeList of
    [] ->
      erlang:send(Requester,{error, no_available_db_node});
    _ ->
      LoginDbNode = lists:nth(rand:uniform(erlang:length(LoginNodeList)), LoginNodeList),
      erlang:send(Requester, {one_node,LoginDbNode})
  end,
  {noreply,State};

handle_info({request_login_db_nodes,Requester}, State) ->
  lager:info( "request_login_db_nodes, Request: ~p", [Requester]),
  LoginNodeList = maps:get(login_db_node_list,State, []),
  erlang:send(Requester, {to_agent, login_db_nodes, LoginNodeList}),
  {noreply, State};

handle_info({nodedown,Node}, State) ->
  lager:info("{nodedown,Node}: Node: ~p", [ Node ]),
  NewState = maps:update_with(login_db_node_list,fun(OldList) -> lists:delete(Node, OldList) end, State),
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
