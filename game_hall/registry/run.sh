#!/usr/bin/env bash

erl -pa ebin -pa $(find ../../deps/lager -name ebin) -name r@127.0.0.1 -detached \
-s user_default start_registry_server

