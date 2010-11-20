"quarice" <-
function(f, para, xmax=NULL) {
   if(! are.parrice.valid(para)) return()
   V <- para$para[1]
   A <- para$para[2]
   if(V == 0) {
      ray <- vec2par(c(0,A), type="ray")
      return(quaray(f,para=ray))
   }
   SNR <- V/A
   if(SNR > 52) {
      xbar <- A * SNR
      xvar <- A^2; # as SNR --> infinity: 2*A^2 + V^2 - A^2 * SNR^2
      nor  <- vec2par(c(xbar,sqrt(xvar)), type="nor")
      return(quanor(f,para=nor))
   } else if(SNR > 24) {
      L05  <- LaguerreHalf(-V^2/(2*A^2))
      xbar <- A * sqrt(pi/2) * L05
      xvar <- 2*A^2 + V^2 - A^2 * (pi/2) * L05^2
      nor  <- vec2par(c(xbar,sqrt(xvar)), type="nor")
      return(quanor(f,para=nor))
   }
   if(is.null(xmax)) {
      for(ord in (1:10)) {
        test.xmax <- 10^ord*(V+A)
        val <- pdfrice(test.xmax, para)
        if(val <= 100*.Machine$double.eps) {
          xmax <- test.xmax
          break
        }
        ifelse(is.finite(val), xmax <- test.xmax, break)
        ord <- ord + 1
      }
   }
   x <- vector(mode="numeric")
   for(i in 1:length(f)) {
     Fx   <- f[i]
     x[i] <- NA
     if(Fx < 0 | Fx >= 1) {
       warning("invalid nonexceedance probability")
       next
     }
     if(Fx == 0) {
       x[i] <- 0
     } else if(Fx == 1) {
       x[i] <- Inf # is this ok?
     } else {
       try( x[i] <-
         uniroot(function(X,...)
                  return(Fx - cdfrice(X,...)),
                  c(0,xmax), para=para)$root, silent=FALSE)
     }
   }
   return(x)
}
