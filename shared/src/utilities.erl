-module(utilities).

-export([get_cpu_num/0]).

get_cpu_num() ->
  case os:type() of
    {_, linux} ->
      CpuCoreNum = erlang:element(1, string:to_integer(os:cmd("cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1"))) + 1,
      CpuCoreNum;
    _ ->
        1
  end.
