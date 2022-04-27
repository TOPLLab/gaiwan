abstraction inc(){
    shaper doinc(yoloname, a:int[n]) : int[2*n] {
        a[yoloname/2]
    }
}

abstraction map(){
    mapper doinc(i, a:int) : int {
        a+1
    }
}

return a | map() | inc()
