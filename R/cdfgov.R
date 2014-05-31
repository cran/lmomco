"cdfgov" <-
function(x, para) {
    if(! are.pargov.valid(para)) return()

   U <- para$para[1]
   A <- para$para[2]
   B <- para$para[3]
   Bp1  <- B + 1
   nBm1 <- 1 - B
   UpA  <- U + A

   "afunc" <- function(x, f) return(x - quagov(f, para))

   f <- vector(mode="numeric")
   for(i in seq(1,length(x))) {
      X <- x[i]
      if(X == U) {
         f[i] <- 0
      } else if(X == UpA) {
         f[i] <- 1
      } else {
         tmp <- NULL
         try(tmp <- uniroot(afunc, lower=0, upper=1, x=X), silent=TRUE)
         if(is.null(tmp)) {
           f[i] <- NA
         } else {
           f[i] <- tmp$root
         }
      }
   }
   return(f)
}

