"pdfrice" <-
function(x,para) {
   if(! are.parrice.valid(para)) return()
   V   <- para$para[1]
   A   <- V/para$para[2]
   B   <- V/A^2
   Asq <- A^2; Vsq <- V^2
   f   <- vector(mode="numeric")
   for(i in seq(1,length(x))) {
     xi  <- x[i]
     tmp <- xi/Asq * exp( -(xi^2 + Vsq) / (2*Asq) )
     Bo  <- besselI(xi*B, n=0)
     if(is.finite(Bo)) {
       f[i] <- tmp*Bo
     } else {
       f[i] <- 0
     }
   }
   return(f)
}

