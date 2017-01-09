#!/usr/bin/env bash

erl -pa ebin \
    -pa ../../connector/ebin \
    -pa ../../deps/ranch/ebin \
    -pa $(find ../../deps/lager -name ebin) \
    -pa ../../shared/ebin \
    -name game_gateway@127.0.0.1 \
    -s user_default start_game_gateway
