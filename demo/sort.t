abstraction llllll(round:int ,takePer:int) {
    mapper bitonic_select(i, a:tuple(int,int)) : tuple(int, int) {
      if((i%(2^(round+1))) < (2^round)){
          if(a[0] < a[1]) {a} else {tuple(a[1],a[0])}
      } else { -- lower half
          if(a[0] < a[1]) {tuple(a[1],a[0])} else {a}
      }
    }
}

shaper randomizer(i) : int[n]{
    (i * 593) % 1000
}
    randomizer(@fresh(33554432)) |
    25:round {
        (round+1):step {
            @split(2,2^(round - step)) |
            bitonic_select(round,round - step + 1) |
            @join(2,2^(round - step))
        }
    }
