"are.parkur.valid" <-
function(para,nowarn=FALSE) {
    if(! is.kur(para)) return(FALSE)
    A <- para$para[1]
    B <- para$para[2]
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0) {
      warning("Alpha parameter is invalid")
      GO <- FALSE
    }
    if(A <= 0) {
      warning("Beta parameter is invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

