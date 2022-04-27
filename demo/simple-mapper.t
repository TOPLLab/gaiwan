abstraction inc(){
    mapper doinc(i, a:int) : int {
        a+1
    }
}

return a | inc()
