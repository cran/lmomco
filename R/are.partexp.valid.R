"are.partexp.valid" <-
function(para,nowarn=FALSE) {
    if(! is.texp(para)) return(FALSE)
    U <- para$para[1]
    A <- para$para[2]
    if(para$is.uni) {
       if(A <= U) {
         warning("Parameters are invalid as uniform dist for texp dist")
         return(FALSE)
       }
       return(TRUE)
    }
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(U <= 0) {
      warning("Parameter U is invalid, must by > 0")
      GO <- FALSE
    }
    if(A == 0) {
      warning("Parameter 1/A is invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

