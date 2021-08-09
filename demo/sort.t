abstraction bitonic_select(round:int ,takePer:int) {
    mapper bitonic_select_impl(i, a:tuple(int,int)) : tuple(int, int) {
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
    @fresh(33554432) |
    randomizer() |
    25:round {
        (round+1):step {
            bitonic_select(round,round - step + 1)
        }
    }
