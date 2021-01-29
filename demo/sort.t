-- Todo before this works:
-- - add index to mapper
-- - nested loops on non-fixed length
-- - Powers and bitshifts
-- - Bitwize operators (now with divisions)
-- - readFile


-- Make 2 buffers, second one 2^takePer posions ahead (modulo)
shuffe bitonic_shuff(takePer, A, ALen){
   A[i] ; A[i + 2^takePer % ALen]
}

mapper bitonic_select(i, takePer, a, b){
  if(i%(2^(takePer+1)) < (2^takePer)){
      -- low high
      if(a < b) {a;b} else {b;a}
  } else {
      -- high low
      if(a < b) {b;a} else {a;b}
  }
}

@readFile("data") |
    4:round {
        (round+1):step {
            bitonic_shuff(round - step + 1) |
            bitonic_select(round - step + 1)
        }
    }
