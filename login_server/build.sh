#!/bin/sh

[ -d ebin ] || mkdir -pv ebin
erl -pz $(find ../deps/lager -name "ebin") -make
cp -avf app/*.app ./ebin

