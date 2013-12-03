"quagpa" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.pargpa.valid(para)) return()
    }
    XI <- para$para[1]
    A  <- para$para[2]
    K  <- para$para[3]
    n <- length(f)
    x <- vector(mode="numeric",length=n)
    for(i in seq(1,n)) {
      if(f[i] == 0)         { x[i] <- XI; next }
      if(f[i] == 1 & K > 0) { x[i] <- XI+A/K; next }
      Y <- -log(1 - f[i])
      if(K != 0) {
        Y <- ( 1 - exp(-K*Y) ) / K
        x[i] <- XI+A*Y
      } else {
        x[i] <- XI + A*Y
      }
    }
    return(x)
}

