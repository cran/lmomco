"are.parcau.valid" <-
function(para,nowarn=FALSE) {
    if(! is.cau(para)) return(FALSE)
    U <- para$para[1] 
    A <- para$para[2] 
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0) {
      warning("Parameters are invalid")
      GO <- FALSE
    }
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}
