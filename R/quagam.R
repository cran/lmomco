"quagam" <-
function(f,para) { 
    if(! are.pargam.valid(para)) return()
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 
    if(f <= 0 || f >= 1) {
      warning("argument of function is invalid")
      return()
    }
    if(f == 0) return(0)
    return(qgamma(f,ALPHA,scale=BETA))
}

