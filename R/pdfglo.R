"pdfglo" <-
function(x,para) {
    if(! are.parglo.valid(para)) return()
    SMALL <- 1e-15 
    XI <- para$para[1] 
    A  <- para$para[2] 
    K  <- para$para[3] 

    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      Y  <- (x[i]-XI)/A 
      if(K == 0) {
        f[i] <- A^-1 * exp(-(1-K)*Y) / (1+exp(-Y))^2
        next
      }
      ARG <- 1-K*Y 
      if(ARG > SMALL) {
        Y <- -log(ARG)/K
        f[i] <- A^-1 * exp(-(1-K)*Y) / (1+exp(-Y))^2
        next
      }
      if(K < 0) { f[i] <- 0; next }
      if(K > 0) { f[i] <- 1; next }
      warning("Should not be here in execution")
    }
    return(f)
}

