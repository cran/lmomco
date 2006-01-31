"cdfgpa" <-
function(x,para) {
    if(! are.pargpa.valid(para)) return()
    #  SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT
    #  THE ENDPOINT OF THE DISTRIBUTION
    SMALL <- 1e-15
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
    Y <- (x-XI)/A
    if(Y <= 0) return(0)
    if(K == 0) {
      return(1-exp(-Y))
    }
    else {
      ARG <- 1-K*Y
      if(ARG > SMALL) {
        Y <- -log(ARG)/K
        return(1-exp(-Y))
      }
      return(1)
    }
}

