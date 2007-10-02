"are.parwak.valid" <-
function(para,nowarn=FALSE) {
    if(! is.wak(para)) return(FALSE)

    A <- para$para[2]
    B <- para$para[3]
    C <- para$para[4]
    D <- para$para[5]
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(B+D <= 0 & (B != 0 | C != 0 | D != 0)) {
       warning("Parameters are invalid")
       GO <- FALSE
    }
    if(A == 0 & B != 0) {
       warning("Parameters are invalid")
       GO <- FALSE
    }
    if(C == 0  & D != 0) {
       warning("Parameters are invalid")
       GO <- FALSE
    }
    if(C < 0 | A+C < 0) {
       warning("Parameters are invalid")
       GO <- FALSE
    }
    if(A == 0 & C == 0) {
       warning("Parameters are invalid")
       GO <- FALSE
    }
    if(D >= 1) {
       warning("Parameters are invalid")
       GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

