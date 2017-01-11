-module(game_gateway_handle).
%% API
-export([]).

-include("game.hrl").

handle(Message,State) ->
  lager:info("~p try to handle Message:~p when State: ~p",[?MODULE,Message,State]),
  handle2(Message,State).

handle2(Message, State) when
