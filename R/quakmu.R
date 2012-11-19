"quakmu" <-
function(f, para, paracheck=TRUE, marcumQ=TRUE, ...) {
   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     if(! are.parkmu.valid(para)) return()
   }

   LARGE <- sqrt(.Machine$double.xmax)

   #HIGH.ENOUGH <- 1 - 1E-6
   #SMALL.POWER <- -9
   #order <- SMALL.POWER; xmax <- 10^order
   #while(1) {
   #   order <- order + 1;
   #   F <- cdfkmu(xmax, para)
   #   if(is.na(F)) break
   #   if(length(F) == 0) break
   #   if(F >= HIGH.ENOUGH) break
   #   xmax <- 10^order
   #}

   x <- vector(mode="numeric")
   for(i in 1:length(f)) {
     Fx   <- f[i]
     x[i] <- NA
     if(Fx < 0 | Fx > 1) {
       warning("invalid nonexceedance probability")
       next
     }
     if(Fx == 0) {
       x[i] <- 0
     } else if(Fx == 1) {
       x[i] <- Inf # is this ok?
     } else {
       int1 <- NULL
       try( int1 <-
         uniroot(function(X,...)
                  return(Fx - cdfkmu(X,...)),
                  c(0,LARGE), para=para)$root, silent=FALSE, ...)
       if(is.null(int1)) { x[i] <- NA; next }
       x[i] <- int1
     }
   }
   return(x)
}
