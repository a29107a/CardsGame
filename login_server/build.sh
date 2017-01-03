#!/bin/sh

[ -d ebin ] || mkdir -pv ebin
erl -pa $(find ../deps/lager -name ebin) -make
cp -avf app/*.app ./ebin

