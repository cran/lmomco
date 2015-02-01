"quagev" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.pargev.valid(para)) return()
    }
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    x <- vector(mode="numeric", length=length(f))
    for(i in seq(1,length(f))) {
      if(f[i] == 0 & K < 0) { x[i] <- XI+A/K; next }
      if(f[i] == 1 & K > 0) { x[i] <- XI+A/K; next }
      Y <- -log(-log(f[i])) 
      if(K != 0) Y <- (1-exp(-K*Y))/K 
      x[i] <- XI+A*Y
    }
    return(x)
}

