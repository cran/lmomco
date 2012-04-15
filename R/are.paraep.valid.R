"are.paraep.valid" <-
function(para,nowarn=FALSE) {
    if(! is.aep(para)) return(FALSE)

    A <- para$para[2]
    K <- para$para[3]
    H <- para$para[4]

    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0) {
      warning("Parameters are invalid")
      GO <- FALSE
    }
    if(K <= 0) {
      warning("Parameters are invalid")
      GO <- FALSE
    }
    if(H <= 0) {
      warning("Parameters are invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}
