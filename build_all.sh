#!/usr/bin/env bash

cd proto
rm -fr include ebin src/pb include && sh generate_protocol.sh
cd ..

cd shared
rm -fr ebin && sh build.sh &
cd ..

cd connector
rm -fr ebin && sh build.sh &
cd ..

cd db_server
rm -fr ebin && sh build.sh &
cd ..


#^^^^^^^^^^^^^^^^^^^^^^ame_hall
cd game_hall

cd registry
rm -fr ebin && sh build.sh &
cd ..

cd login_server
rm -fr ebin && sh build.sh &
cd ..

cd game_server_center
rm -fr ebin && sh build.sh &
cd ..

cd ..
#$$$$$$$$$$$$$$$$$$$game_hall

#^^^^^^^^^^^^^^^^^^^games
cd games

cd game_center_agent
rm -fr ebin && sh build.sh &
cd ..

cd game_gateway
rm -fr ebin && sh build.sh &
cd ..

cd robot
rm -fr robot && sh build.sh &
cd ..

cd ..
#$$$$$$$$$$$$$$$$$$$games

echo "all building process started!"
echo ""
