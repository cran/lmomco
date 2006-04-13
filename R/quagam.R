"quagam" <-
function(f,para) { 
    if(! are.pargam.valid(para)) return()
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 

    x <- vector(mode="numeric")
    for(i in seq(1,length(f))) {
      if(f[i] <= 0 || f[i] >= 1) {
        warning("argument of function is invalid")
        return()
      }
      if(f[i] == 0) { x[i] <- 0; next }
      x[i] <- qgamma(f[i],ALPHA,scale=BETA)
    }
    return(x)
}

