"are.parst3.valid" <-
function(para,nowarn=FALSE) {
    if(! is.st3(para)) return(FALSE)
    U <- para$para[1]
    A <- para$para[2]
    N <- para$para[3]
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0) {
      warning("Parameter Alpha is invalid")
      GO <- FALSE
    }
    if(N <= 1) {
      warning("Parameter Nu is invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

