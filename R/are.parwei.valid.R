"are.parwei.valid" <-
function(para,nowarn=FALSE) {
    if(! is.wei(para)) return(FALSE)

    ZETA <- para$para[1]
    B <- para$para[2]
    D <- para$para[3]

    K <- 1/D
    A <- B/D
    XI <- ZETA - B 

    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0 | K <= -1) {
      warning("Parameters are invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

