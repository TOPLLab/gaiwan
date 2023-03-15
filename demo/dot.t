

abstraction dot_product() {
    shaper join(i,a:C[n],b:C[n]) : tuple(C,C)[n] {
        tuple(a[i],b[i])
    } |
    mapper mul(i, a:tuple(int,int)) : int {
        a[[0]]*a[[1]]
    } |
    reducer sum(i,acc: int , d : int) : int[1] (0){
        d + acc
    }
}

returnab  | dot_product()
