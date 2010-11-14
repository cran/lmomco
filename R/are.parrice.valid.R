"are.parrice.valid" <-
function(para,nowarn=FALSE) {
    if(! is.rice(para)) return(FALSE)
    V   <- para$para[1]
    A   <- V/para$para[2]
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0) {
      warning("Parameter A is invalid, poor SNR?")
      GO <- FALSE
    }
    if(V < 0) { # V = 0 is Rayleigh
      warning("Parameter V is invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

