"are.parln3.valid" <-
function(para,nowarn=FALSE) {
    if(! is.ln3(para)) return(FALSE)
    A <- para$para[3]
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

