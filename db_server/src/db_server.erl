-module(db_server).

-behaviour(gen_server).
-export([start_link/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  erlang:send(erlang:self(), establish_db_connection),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(establish_db_connection, State) ->
  establish_db_connection(),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

establish_db_connection() ->
  {ok,ConfigList} = file:consult("config/db.config"),
  lists:foreach(fun({_DatabaseName,DatabaseConnectionConfig}) ->
    Seed = proplists:get_value(seed,DatabaseConnectionConfig,{single,"127.0.0.1:27017"}),
    ConnectionList = proplists:get_value(connection, DatabaseConnectionConfig, []),
    WorkerOptions = proplists:get_value(worker_options, DatabaseConnectionConfig,[{database,<<"test">>}]),
    IndexOptions = proplists:get_value(index_options,DatabaseConnectionConfig,[]),
    {ok, TopologyPid} = mongoc:connect(Seed, ConnectionList,WorkerOptions),
    lists:foreach(fun({IndexCollection, Index}) ->
      mongo_api:ensure_index(TopologyPid, IndexCollection, Index)
                  end,
      IndexOptions)
                end,
    ConfigList).


