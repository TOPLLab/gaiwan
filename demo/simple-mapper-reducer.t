abstraction inc(){
    mapper doinc(i, a:int) : int {
        a+1
    }
    |
    reducer sum(i,acc: int , d : int) : int[1] (0){
        d + acc
    }
}

return a | inc()
