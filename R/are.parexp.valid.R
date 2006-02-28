"are.parexp.valid" <-
function(para) {
    if(! is.exp(para)) return(FALSE)
    A <- para$para[2]
    if(A <= 0) {
      warning("Parameters are invalid")
      return(FALSE)
    }
    return(TRUE)
}

