"are.parpe3.valid" <-
function(para) {
    if(! is.pe3(para)) return(FALSE)
    A <- para$para[2]
    if(A <= 0) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

