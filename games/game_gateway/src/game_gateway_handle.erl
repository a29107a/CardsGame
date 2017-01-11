-module(game_gateway_handle).
%% API
-export([handle/2]).

-include("game_pb.hrl").

handle(Message,State) ->
  lager:info("~p try to handle Message:~p when State: ~p",[?MODULE,Message,State]),
  handle2(Message,State).

handle2(Message, State) ->
  lager:error( "~p unhandled message, Message:~p State:~p",[?MODULE,Message,State]).
