"are.pargam.valid" <-
function(para) {
    if(! is.gam(para)) return(FALSE)
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 
    if(ALPHA <= 0 | BETA <= 1) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

