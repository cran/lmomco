"quaemu" <-
function(f, para, paracheck=TRUE, yacoubsintegral=TRUE, eps=1e-7) {
   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     if(! are.paremu.valid(para)) return()
   }

   xmu <- lmomemu(para, nmom=1)$lambdas[1]
  "quaemu.recur" <- function(f, para, x) {
      if(x < 0) {
         x <- ifelse(abs(x) > xmu, xmu/2, abs(x)/2)
      }
      Fx <- cdfemu(x, para, paracheck=FALSE,
                   yacoubsintegral=yacoubsintegral)
      tmp <- f - Fx # compute once, use twice
      #message("x = ",x, "  F = ",f, "  tmp=",tmp)
      if(abs(tmp) < eps) { # very close in probability
        names(x) <- NULL
        return(x) # stop recursion and return
      } else {
        fx <- pdfemu(x, para, paracheck=FALSE) # PDF of the lmomco package
        newx <- x + tmp/fx # as seen in the equation
        x.np1 <- quaemu.recur(f, para, newx)
        return(x.np1)
      }
   }

   x <- vector(mode="numeric", length=length(f))
   for(i in 1:length(f)) {
     Fx <- f[i]
     if(Fx == 0) {
       x[i] <- 0
     } else {
       qua <- quaemu.recur(Fx, para, xmu)
       x[i] <- qua
     }
   }
   return(x)
}

