"quagam" <-
function(f,para,paracheck=TRUE) { 
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.pargam.valid(para)) return()
    }
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 
    x <- vector(mode="numeric", length=length(f))
    for(i in seq(1,length(f))) {
      if(f[i] == 0) { x[i] <- 0; next }
      x[i] <- qgamma(f[i],ALPHA,scale=BETA)
    }
    return(x)
}

