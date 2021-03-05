mapper id(i, a, b){
    a ; b
}

mapper idt(i, a, b){
    10*a ; 100*b
}

mapper id1(i, a){
    a
}

@generateSeq(1,8) | @split(2,1)  | idt() | @join(2,2)
