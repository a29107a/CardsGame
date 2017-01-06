-module(utilities).

-compile(export_all).

get_cpu_num() ->
  case os:type() of
    {_, linux} ->
      CpuCoreNum = erlang:element(1, string:to_integer(os:cmd("cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1"))) + 1,
      CpuCoreNum;
    _ ->
        1
  end.

list_random_one(List) when erlang:length(List) > 0 ->
  lists:nth(rand:uniform(erlang:length(List)), List);
list_random_one(_) -> [].

list_random_one_with_weight(List) when erlang:length(List) > 0 ->
  {WeightList, TotalWeight} =
  lists:mapfoldl(fun({Element, Weight}, Acc) ->
    {{Element, Weight+Acc}, Weight+Acc}
                 end,
    0,
    List),
  R = rand:uniform(TotalWeight),
  list_random_one_with_weight(WeightList, R).

list_random_one_with_weight(_) ->
  [].

list_random_one_with_weight([], _R) ->
  {error, not_found};
list_random_one_with_weight(WeightList, R) ->
  Hd = erlang:hd(WeightList),
  Weight = erlang:element(2, Hd),
  Element = erlang:element(1, Hd),
  case R =< Weight of
    true ->
      Element;
    false ->
      list_random_one_with_weight(erlang:tl(WeightList),R)
  end.
