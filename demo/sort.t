-- Todo before this works:
-- - √ add index to mapper
-- - √ nested loops on non-fixed length
-- - Powers and bitshifts
-- - Bitwize operators (now with divisions)
-- - readFile


-- Make 2 buffers, second one 2^takePer posions ahead (modulo)
shuffe bitonic_shuff(takePer, A, ALen){
   A[2*i] ; A[2*i + 1]
}

mapper bitonic_select(i, takePer, a, b){
  if(i%(2^(takePer+1)) < (2^takePer) && a < b){
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
            @chunkBy(round - step + 1) |
            bitonic_select(round - step + 1) |
            @join()
        }
    }
