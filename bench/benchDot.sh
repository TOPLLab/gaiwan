#!/usr/bin/env bash

target="/tmp/gurdee81/prog"

if [ -f "~/loadstack" ] ;
then
    source ~/loadstack
fi

if [ -d "$(dirname "$target")" ] ;
then
    echo "Target folder exist ($target)"
else
    echo "Target folder does not exist ($target)"
    exit 1
fi



curFile=$(realpath "$0")
curDir=$(dirname "$curFile")

cd $curDir/..

stack run --no-nix eval demo/simple-mapper.t

for i in {13..27};
do
size=$(echo "2^$i" | bc)
f="/tmp/gurdee81/datafiles/size$size.int.gw"
echo "=========================================="
echo "starting dot $i : $size"
date
echo "------------------------------------------"
rm -f demo/input/a.int.gw
rm -f demo/input/b.int.gw
ln -s "$f" demo/input/a.int.gw
ln -s "$f" demo/input/b.int.gw

stdbuf -oL -eL stack --no-nix run -- eval demo/dot.t 2>&1 |\
    stdbuf -oL -eL  awk '{t=strftime("%s"); print (t-'$(date +%s)') "|" $0 }'
done | tee -a /imec/users/gurdee81/bechdata

