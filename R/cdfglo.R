"cdfglo" <-
function(x,para) {
    if(! are.parglo.valid(para)) return()
    SMALL <- 1e-15 
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    Y  <- (x-XI)/A 
    if(K == 0) {
      return(1/(1+exp(-Y)))
    }
    ARG <- 1-K*Y 
    if(ARG > SMALL) {
      Y <- -log(ARG)/K
      return(1/(1+exp(-Y)))
    }
    if(K < 0) return(0)
    if(K > 0) return(1)
}

