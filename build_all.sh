#!/usr/bin/env bash

cd proto
rm -frv include ebin src/pb include
sh generate_protocol.sh
cd ..

cd shared
rm -frv ebin
sh build.sh
cd ..

cd connector
rm -frv ebin
sh build.sh
cd ..

cd db_server
rm -frv ebin
sh build.sh
cd ..

cd game_hall

cd login_server
rm -frv ebin
sh build.sh
cd ..

cd game_server_center
rm -frv ebin
sh build.sh
cd ..

cd ..

cd registry
rm -frv ebin
sh build.sh
cd ..
