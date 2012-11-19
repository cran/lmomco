"are.parkmu.valid" <-
function(para, nowarn=FALSE) {
    if(! is.kmu(para)) return(FALSE)
    K   <- para$para[1]
    M   <- para$para[2]
    m   <- (M*(1+K)^2)/(1+2*K)
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(K < 0) {
      warning("Parameter KAPPA is invalid")
      GO <- FALSE
    }
    if(M < 0) {
      warning("Parameter MU is invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

