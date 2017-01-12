#!/usr/bin/env bash

erl -pa ebin \
    -pa $(find ../../deps/lager -name ebin) \
     -pa ../../shared/ebin \
    -name game_center_agent@127.0.0.1 -detached \
    -s user_default start_game_center_agent

