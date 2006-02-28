"are.pargev.valid" <-
function(para) {
    if(! is.gev(para)) return(FALSE)
    A <- para$para[2]
    G <- para$para[3]
    if(A <= 0 | G <= -1) {
      warning("Parameters are invalid")
      return(FALSE)
    }
    return(TRUE)
}

