abstraction llllll(round:int ,takePer:int) {
    mapper bitonic_select(i, a:tuple(int,int)) : tuple(int, int) {
      if((i%(2^(round+1))) < (2^round)){
          if(a[1] < a[2]) {a} else {tuple(a[2],a[1])}
      } else { -- lower half
          if(a[1] < a[2]) {tuple(a[2],a[1])} else {a}
      }
    }
}

mapper randomizer(i){
    (i * 593) % 1000
}
@generateSeq(1,33554432) |
    randomizer() |
    25:round {
        (round+1):step {
            @split(2,2^(round - step)) |
            bitonic_select(round,round - step + 1) |
            @join(2,2^(round - step))
        }
    }
