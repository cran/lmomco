"pdfgov" <-
function(x,para) {

   if(! are.pargov.valid(para)) return()

   U <- para$para[1]
   A <- para$para[2]
   B <- para$para[3]

   Bp1  <- B + 1
   nBm1 <- 1 - B
   UpA  <- U + A
   tmp1  <- 1 / (A*B*Bp1)

   f <- vector(mode="numeric")
   for(i in seq(1,length(x))) {
      X <- x[i]
      Fx <- cdfgov(X, para) # cdfgov traps the end points
      tmp2 <- tmp1 * Fx^nBm1 / (1 - Fx) # just fine
      f[i] <- ifelse(is.finite(tmp2), tmp2, NA) # so that this test suffices
   }
   return(f)
}


