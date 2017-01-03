#!/usr/bin/env bash

[ -d ebin ] || mkdir -pv ebin
erl -make
cp -avf app/*.app ebin
