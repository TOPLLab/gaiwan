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
echo "=========================================="
echo "starting $i : $size"
date
echo "------------------------------------------"
cat <<GAIWAN > $target
abstraction bitonic_select(round:int , arrPerBlock:int) {
    shaper split(i,d:C[2*n]) : tuple(C,C)[n] {
        let blockid = i/arrPerBlock in
            let blockstart = blockid * arrPerBlock * 2 in
            let blockoffset = i % arrPerBlock in
            let pos = blockstart + blockoffset in
            tuple(d[pos],d[pos+arrPerBlock])
    } |
    mapper bitonic_select_impl(i, a:tuple(int,int)) : tuple(int, int) {
        if((i%(2^(round+1))) < (2^round)){
            if(a[[0]] < a[[1]]) {a} else {tuple(a[[1]],a[[0]])}
        } else { -- lower half
            if(a[[0]] < a[[1]]) {tuple(a[[1]],a[[0]])} else {a}
        }
    } |
    shaper join(i,d:tuple(B,B)[n]) : B[2*n] {
        let arrowBlock = i/(2*arrPerBlock) in
            let arrowBlockStart = arrowBlock * arrPerBlock in
            let arrowOffset = i % arrPerBlock in
            if(arrowBlockStart*2+arrPerBlock < i+1){
                d[arrowBlockStart + arrowOffset][[0]]
            }else{
                d[arrowBlockStart + arrowOffset][[1]]
            }
    }
}

return size$size |
$i:round {
    (round+1):step {
        bitonic_select(round,2^(round - step))
    }
}
GAIWAN
stdbuf -oL -eL stack --no-nix run -- eval $target 2>&1 |\
    stdbuf -oL -eL  awk '{t=strftime("%s"); print (t-'$(date +%s)') "|" $0 }'
done | tee -a /imec/users/gurdee81/bechdata

