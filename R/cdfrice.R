"cdfrice" <- function(x, para=NULL) {
    V   <- para$para[1]
    A   <- para$para[2]
    if(V == 0) {
      ray <- vec2par(c(0,A), type="ray")
      return(cdfray(x,para=ray))
    }
   SNR <- V/A
   if(SNR > 52) {
      xbar <- A * SNR
      xvar <- A^2; # as SNR --> infinity: 2*A^2 + V^2 - A^2 * SNR^2
      nor  <- vec2par(c(xbar,sqrt(xvar)), type="nor")
      return(cdfnor(x,para=nor))
   } else if(SNR > 24) {
      L05  <- LaguerreHalf(-V^2/(2*A^2))
      xbar <- A * sqrt(pi/2) * L05
      xvar <- 2*A^2 + V^2 - A^2 * (pi/2) * L05^2
      nor  <- vec2par(c(xbar,sqrt(xvar)), type="nor")
      return(cdfnor(x,para=nor))
   }
   f <- vector(mode="numeric")
   for(i in 1:length(x)) {
   	 if(x[i] < 0) {
     	f[i] <- 0
     } else if(x[i] == Inf) {
       f[i] <- 1
     } else {
         f[i] <- integrate(pdfrice,
                           0, x[i],
                           para=para)$value
     }
   }
   return(f)
}
