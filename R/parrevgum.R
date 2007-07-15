"parrevgum" <-
function(lmom,zeta=1) {
   # L-moments need to be Type-B
   euler <- 0.577215664901532861

   # Exponential Integral as defined by Hosking (1995, p. 558, A.9)
   Ei <- function(X) {
          z <- integrate(function(x){ x^-1*exp(-x) },
                         lower=X,
                         upper=Inf);
                         return(z)
   }


   para <- vector(mode="numeric", length=2)

   if(length(lmom$L1) == 1) { # convert to unnamed L-moments
     lmom <- lmorph(lmom)     # nondestructive conversion!
   }
   if(! are.lmom.valid(lmom)) {
     warning("B-type L-moments are invalid")
     return()
   }
   str(lmom)
   zc <- 1 - zeta

   z1 <- list(value = 0)
   z2 <- list(value = 0)

   if(zc != 0) {
     z1 <- Ei(-log(zc))
     z2 <- Ei(-2*log(zc))
   }
   

   tmp <- log(2) + z2$value - z1$value
   para[2] <- lmom$lambdas[2]/tmp

   tmp <- euler + z1$value
   para[1] <- lmom$lambdas[1] + tmp*para[2]
 
   return(list(type = 'revgum', para=para, zeta=zeta, source="parrevgum"))
}

