#!/usr/bin/env bash

erl -pa ebin -pa ../../connector/ebin \
  -pa ../../deps/ranch/ebin/ \
  -pa ../../shared/ebin \
  -pa $(find ../../deps/lager -name ebin) \
  -name game_server_center@127.0.0.1 -detached  \
  -s user_default start_game_server_agent_server
