#!/usr/bin/env bash

cd proto
sh generate_protocol.sh
cd ..

cd shared
sh build.sh
cd ..

cd connector
sh build.sh
cd ..

cd db_server
sh build.sh
cd ..

cd game_hall

cd login_server
sh build.sh
cd ..

cd game_server_center
sh build.sh
cd ..

cd ..

cd registry
sh build.sh
cd ..
