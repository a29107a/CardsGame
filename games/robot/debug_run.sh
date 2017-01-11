#!/usr/bin/env bash

erl -pa ebin \
    -pa $(find ../../deps/lager -name ebin) \
    -pa ../../shared/ebin \
    -name robot@127.0.0.1 \
    -s user_default start_robot
