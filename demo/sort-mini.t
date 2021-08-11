mapper bitonic_select(i, round:int ,takePer:int, a:tuple(int,int)) : tuple(int, int) {
  if((i%(2^(round+1))) < (2^round)){
      if(a[1] < a[2]) {a} else {tuple(a[2],a[1])}
  } else { -- lower half
      if(a[1] < a[2]) {tuple(a[2],a[1])} else {a}
  }
}

mapper randomizer(i){
    (i * 593) % 100
}

@generateSeq(1,8) |
    randomizer() |
    3:round {
        (round+1):step {
            @split(2,2^(round - step)) |
            bitonic_select(round,round - step + 1) |
            @join(2,2^(round - step))
        }
    }
