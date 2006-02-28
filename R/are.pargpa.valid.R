"are.pargpa.valid" <-
function(para) {
    if(! is.gpa(para)) return(FALSE)
    A <- para$para[2]
    K <- para$para[3]
    if(A <= 0 | K < -1) {
       warning("Parameters are invalid")
       return(FALSE)
    }
    return(TRUE)
}

