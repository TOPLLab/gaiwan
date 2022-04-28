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
            if(arrowBlockStart*2+arrPerBlock < i){
                d[arrowBlock * arrPerBlock + arrowOffset][[0]]
            }else{
                d[arrowBlock * arrPerBlock + arrowOffset][[1]]
            }
    }
}

return a | bitonic_select(0,1)
-- return a |
-- 2:round {
--     (round+1):step {
--         bitonic_select(round,2^(round - step))
--     }
-- }
