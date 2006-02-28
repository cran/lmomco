"are.parglo.valid" <-
function(para) {
    if(! is.glo(para)) return(FALSE)
    A  <- para$para[2] 
    K  <- para$para[3] 
    if(A <= 0 | abs(K) >= 1) {
      warning("Parameters are invalid")
      return(FALSE)
    }
    return(TRUE)
}

