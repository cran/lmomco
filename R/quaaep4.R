"quaaep4" <-
function(f, para, paracheck=TRUE) {

   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     if(! are.paraep4.valid(para)) return()
   }

   U <- para$para[1]
   A <- para$para[2]
   K <- para$para[3]
   H <- para$para[4]

   # The following appears unnecessary as the AEP seemingly sweeps through
   # either the Normal or Laplace distributions as the K or H pass near the
   # critical points. Hence, all of this code is commented out.
   #SMALL <- 1E-6
   #if(abs(K - 1) < SMALL & abs(H - 2) < SMALL) {
   #   if(warn) warning("Normal distribution being used for k=", K, " and h=", H)
   #   SIGMA <- 0.39894228 * sqrt(pi) * A
   #   MU    <- U
   #   return(quanor(f, vec2par(c(MU, SIGMA), type="nor")))
   #}
   #if(abs(K - 1) < SMALL & abs(H - 1) < SMALL) {
   #   if(warn) warning("Laplace distribution being used for k=", K, " and h=", H)
   #   A  <- A # L2lap = 0.75 * Alap;  L2aep = 0.75 * Aaep
   #   XI <- U
   #   return(qualap(f, vec2par(c(XI, A), type="lap")))
   #}

   H1   <- 1/H
   K2p1 <- (1 + K*K)
   AK   <- A*K
   AoK  <- A/K
   K2p1oKK <- K2p1/(K*K)

   F.of.XI <- cdfaep4(U, para)

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

