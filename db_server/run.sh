#!/usr/bin/env bash

 erl -pa ebin -pa ../shared/ebin -pa $(find ../deps/mongodb-erlang -name ebin) -pa $(find ../deps/lager -name ebin) \
 -name db@127.0.0.1 -detached -s user_default start_db_server
