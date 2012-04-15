"quaaep" <-
function(f, para, paracheck=TRUE) {

   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     if(! are.paraep.valid(para)) return()
   }

   U <- para$para[1]
   A <- para$para[2]
   K <- para$para[3]
   H <- para$para[4]

   H1   <- 1/H
   K2p1 <- (1 + K*K)
   AK   <- A*K
   AoK  <- A/K
   K2p1oKK <- K2p1/(K*K)

   F.of.XI <- cdfaep(U, para)

   x <- vector(mode="numeric", length=length(f))
   for(i in 1:length(f)) {
     Fx   <- f[i]
     x[i] <- NA

     if(Fx == 0) {
       x[i] <- -Inf # is this ok?
     } else if(Fx == 1) {
       x[i] <- Inf  # is this ok?
     } else if(Fx < F.of.XI) {
         x[i] <- U - AK*(qgamma(K2p1oKK*Fx, H1, lower.tail=FALSE))^H1
     } else if(Fx >= F.of.XI) {
         x[i] <- U + AoK*(qgamma(K2p1*(1-Fx), H1, lower.tail=FALSE))^H1
     } else {
       warning("should not be here")
       x[i] <- NA
     }
   }
   return(x)
}

