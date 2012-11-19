"are.paremu.valid" <-
function(para, nowarn=FALSE) {
    if(! is.emu(para)) return(FALSE)
    E <- para$para[1]
    M <- para$para[2]
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(E < 0) {
      warning("Parameter ETA is invalid")
      GO <- FALSE
    }
    if(E > 1) {
      warning("Parameter ETA is invalid")
      GO <- FALSE
    }
    if(M <= 0) {
      warning("Parameter MU is invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

