"cdfgev" <-
function(x,para) {
    if(! are.pargev.valid(para)) return()
    # SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT 
    # THE ENDPOINT OF THE DISTRIBUTION 
    SMALL <- 1e-15 

    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    Y  <- (x - XI)/A
    if(K == 0) return(exp(-exp(-Y)))
    ARG <- 1-K*Y 
    if(ARG > SMALL) {
      Y <- -log(ARG)/K
      return(exp(-exp(-Y)))
    } 
    if(K < 0) return(0)
    # K must be greater than zero to return other end 
    return(1)
}

