"cdfkap" <-
function(x,para) {
    if(! are.parkap.valid(para)) return()

    #  SMALL IS A SMALL NUMBER, USED TO TEST WHETHER X IS
    #  EFFECTIVELY AT AN ENDPOINT OF THE DISTRIBUTION
    SMALL <- 1e-15

    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]
    
    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      Y <- (x[i]-U)/A
      if(G == 0) {
        Y <- exp(-Y)
      }
      else {
        ARG <- 1-G*Y
        if(ARG > SMALL) { 
          Y <- -log(ARG)/G
          Y <- exp(-Y)
        }
        else {
          if(G < 0) { f[i] <- 0; next }
          if(G > 0) { f[i] <- 1; next }
          stop("should not be here in execution")
        }
     }
     if(H == 0) {
       f[i] <- exp(-Y)
     }
     else {
       ARG <- 1-H*Y
       if(ARG > SMALL) {
         Y <- -log(ARG)/H
         f[i] <- exp(-Y)
       }
       else {
         f[i] <- 0
       }
     }
   }
  return(f)
}

