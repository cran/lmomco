"are.pargpa.valid" <-
function(para,nowarn=FALSE) {
    if(! is.gpa(para)) return(FALSE)
    A <- para$para[2]
    K <- para$para[3]
    op <- options()
    GO <- TRUE
    if(nowarn == TRUE) options(warn=-1)
    if(A <= 0 | K <= -1) {
       warning("Parameters are invalid")
       GO <- FALSE
    }
    return(TRUE)
    options(op)
    if(GO) return(TRUE)
    return(FALSE)
}

