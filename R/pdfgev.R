"pdfgev" <-
function(x,para) {
    if(! are.pargev.valid(para)) return()
    # SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT 
    # THE ENDPOINT OF THE DISTRIBUTION 
    SMALL <- 1e-15 

    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 
 
    f <- vector(mode = "numeric")
    for(i in seq(1,length(x))) {
      Y  <- (x[i] - XI)/A
      if(K == 0) {
      	f[i] <- A^(-1) * exp(-(1-K)*y-exp(-Y))
      	next
      }
      ARG <- 1-K*Y 
      if(ARG > SMALL) {
        Y <- -log(ARG)/K
        f[i] <- A^(-1) * exp(-(1-K)*Y-exp(-Y))
        next
      } 
      if(K < 0) {
      	f[i] <- 0
      	next
      }
      # K must be greater than zero so return other end 
      f[i] <- 1
    }
    return(f)
}

