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
    Y <- (x-U)/A
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
        if(G < 0) return(0)
        if(G > 0) return(1)
        stop("should not be here in execution")
      }
   }
   if(H == 0) {
     return(exp(-Y))
   }
   else {
     ARG <- 1-H*Y
     if(ARG > SMALL) {
       Y <- -log(ARG)/H
       return(exp(-Y))
     }
     else {
      return(0)
     }
   }
}

