mapper bitonic_select(i, round ,takePer, a, b){
  if((i%(2^(round+1))) < (2^round)){ -- upper half
      if(a < b) {a} else {b}
  } else { -- lower half
      if(a < b) {b} else {a}
  };
  if((i%(2^(round+1))) < (2^round)){
      if(a < b) {b} else {a}
  } else {
      if(a < b) {a} else {b}
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
