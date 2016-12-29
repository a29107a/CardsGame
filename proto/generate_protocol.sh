#!/bin/sh

[ -d ebin ] || mkdir -pv ebin
[ -d include ] || mkdir -pv include
[ -d src/pb ] || mkdir -pv src/pb


for f in $(ls *.proto)
do
../deps/gpb/bin/protoc-erl -defaults-for-omitted-optionals -pldefs -I ./ -o-hrl include -o-erl src/pb ${f}
done

erl -make
