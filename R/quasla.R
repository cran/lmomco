"quasla" <-
function(f, para, paracheck=TRUE) {
   doR <- FALSE
   LARGE <- 1E11
   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     if(! are.parsla.valid(para)) return()
   }
   U <- para$para[1]
   A <- para$para[2]

   x <- vector(mode="numeric", length=length(f))
   for(i in 1:length(f)) {
     Fx   <- f[i]
     x[i] <- NA
     if(Fx == 0) {
       x[i] <- -Inf # is this ok?
     } else if(Fx == 1) {
       x[i] <-  Inf # is this ok?
     } else if(doR) {
       warning("A native R method not found yet")
     } else {
        try( x[i] <- optimize(function(X,...)
                              return(abs(Fx - cdfsla(X,...))),
                              c(-LARGE,
                                 LARGE), para=para)$minimum,
             silent=FALSE)
     }
   }
   return(x)
}
