"quagov" <-
function(f, para, paracheck=TRUE) {
   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     #if(! are.pargov.valid(para)) return()
   }
   U <- para$para[1]
   A <- para$para[2]
   B <- para$para[3]
   Bp1 <- B + 1

   x <- vector(mode="numeric", length=length(f))
   for(i in 1:length(f)) {
     Fx   <- f[i]
     x[i] <- NA
     if(Fx == 0) {
       x[i] <- U
     } else if(Fx == 1) {
       x[i] <- U + A
     } else {
       x[i] <- U + A*(Bp1*Fx^B - B*Fx^Bp1)
     }
   }
   return(x)
}

