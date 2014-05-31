"are.pargov.valid" <-
function(para,nowarn=FALSE) {
    if(! is.gov(para)) return(FALSE)
    A <- para$para[2]
    B <- para$para[3]
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0) {
      warning("Parameter A is invalid")
      GO <- FALSE
    }
    if(B <= 0) {
      warning("Parameter B is invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

