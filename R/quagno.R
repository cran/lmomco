"quagno" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.pargno.valid(para)) return()
    }
    ZERO <- 1e-6
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3]
    n <- length(f)
    x <- vector(mode="numeric",length=n)
    for(i in seq(1,n)) {
      if(f[i] == 0 & K < 0) { x[i] <- XI+A/K; next }
      if(f[i] == 1 & K > 0) { x[i] <- XI+A/K; next }
      Y <- qnorm(f[i])
      if(abs(K) > ZERO) Y <- (1 - exp(-K*Y) ) / K
      x[i] <- XI+A*Y
    }
    return(x)
}

