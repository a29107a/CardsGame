-module(login_server_uid).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([get/0]).

-define(TIMESTAMP_BIT_LENGTH, 41).
-define(MACHINE_CODE_BIT_LENGTH, 10).
-define(SEQUENCE_BIT_LENGTH, 12).

get() ->
  gen_server:call(?MODULE, get).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{priority, high}]).

init([]) ->
  SystemTimeMilliseconds = erlang:system_time(milli_seconds),
  MachineCode = config_server:get(login_server_config,login_server_id),
  Sequence = 0,
  MaxSequence = erlang:round(math:pow(2,?SEQUENCE_BIT_LENGTH)),
  State = #{system_time_milli_seconds => SystemTimeMilliseconds,machine_code => MachineCode,sequence => Sequence, max_sequence => MaxSequence},
  {ok, State}.

handle_call(get, _From, State) ->
  NowMilliSeconds = erlang:system_time(milli_seconds),
  {Integer, NewState} = generate_one_integer(State,NowMilliSeconds),
  {reply, Integer, NewState};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

generate_one_integer(State, NowSystemTimeMilliSeconds) ->
  #{system_time_milli_seconds := SystemTimeMilliseconds,
    machine_code := MachineCode,
    sequence := Sequence,
    max_sequence := MaxSequence} = State,
  case SystemTimeMilliseconds =:= NowSystemTimeMilliSeconds of
    false ->
      NewState = State#{system_time_milli_seconds => NowSystemTimeMilliSeconds, sequence => 0},
      <<Integer:64>> = <<0:1,NowSystemTimeMilliSeconds:?TIMESTAMP_BIT_LENGTH,
        MachineCode:?MACHINE_CODE_BIT_LENGTH,
        0:?SEQUENCE_BIT_LENGTH>>,
      {Integer, NewState};
    true ->
      case Sequence =:= MaxSequence of
        true ->
          lager:warning_unsafe( "generating too fast, Sequence: ~p", [ Sequence ]),
          generate_one_integer(State, erlang:system_time(milli_seconds));
        false ->
          NextSequence = Sequence + 1,
          NewState = State#{sequence => NextSequence},
          <<Integer:64>> = <<0:1,SystemTimeMilliseconds:41,MachineCode:?MACHINE_CODE_BIT_LENGTH,NextSequence:12>>,
          {Integer, NewState}
      end
  end.
