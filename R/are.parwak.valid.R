"are.parwak.valid" <-
function(para) {
    if(! is.wak(para)) return(FALSE)

    A <- para$para[2]
    B <- para$para[3]
    C <- para$para[4]
    D <- para$para[5]
    if(B+D <= 0 & (B != 0 | C != 0 | D != 0)) {
       warning("Parameters are invalid")
       return(FALSE)
    }
    if(A == 0 & B != 0) {
       warning("Parameters are invalid")
       return(FALSE)
    }
    if(C == 0  & D != 0) {
       warning("Parameters are invalid")
       return(FALSE)
    }
    if(C < 0 | A+C < 0) {
       warning("Parameters are invalid")
       return(FALSE)
    }
    if(A == 0 & C == 0) {
       warning("Parameters are invalid")
       return(FALSE)
    }
    if(D >= 1) {
       warning("Parameters are invalid")
       return(FALSE)
    }
    return(TRUE)
}

