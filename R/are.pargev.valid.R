"are.pargev.valid" <-
function(para,nowarn=FALSE) {
    if(! is.gev(para)) return(FALSE)
    A <- para$para[2]
    G <- para$para[3]
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0 | G <= -1) {
      warning("Parameters are invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

