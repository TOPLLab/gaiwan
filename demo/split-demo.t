mapper id(i, a){
    a
}

mapper idt(i, a, b):tuple(int,int){
    tuple(10*a , 100*b)
}


@generateSeq(1,8) | @split(2,1)  | idt() | @join(2,2)
