-module(db_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  case 'db_sup':start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.


stop(_State) ->
  ok.
