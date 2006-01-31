"quagno" <-
function(f,para) {
    if(! are.pargno.valid(para)) return()
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    if(f <= 0 || f >= 1) {
      if(f == 0 & K < 0) return(XI+A/K)
      if(f == 1 & K > 0) return(XI+A/K)
      warning("argument of function is invalid")
      return()
    }
    Y <- qnorm(f)
    if(K != 0) Y <- (1-exp(-K*Y))/K
    return(XI+A*Y)
}

