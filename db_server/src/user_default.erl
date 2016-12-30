-module(user_default).

-compile(export_all).

start_db_server() ->
  try_to_register_db_node_to_regitry_node(),
  ok = erlang:element(1, application:ensure_all_started(db)).

try_to_register_db_node_to_regitry_node() ->
  {ok, ConfigList} = file:consult("config/db_server.config"),
  RegistryNodeList = proplists:get_value(registry_node_list,ConfigList, []),
  case os:getenv("DEBUG") of
    false ->
      case RegistryNodeList of
        [] ->
          erlang:halt('Cannot register to registry node.');
        _ ->
          PongCount=
            lists:foldl(fun(RegisterNode, Acc) ->
              case net_adm:ping(RegisterNode) of
                pong ->
                  erlang:send({registered_db_server,RegisterNode},{register_login_db_node,erlang:node()}),
                  Acc + 1;
                pang ->
                  Acc
              end
                        end,
              0,
              RegistryNodeList),
          case PongCount > 0 of
            true ->
              next;
            false ->
              erlang:halt('Cannot register to any regitry node.')
          end
      end;
    _ ->
      ignore
  end.
