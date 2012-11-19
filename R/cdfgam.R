"cdfgam" <-
function(x,para) {
    if(! are.pargam.valid(para)) return()
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 
      
    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
      if(x[i] <= 0) { f[i] <- 0; next }
      f[i] <- pgamma(x[i],ALPHA,scale=BETA)
    }
    return(f)
}

