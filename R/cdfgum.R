"cdfgum" <-
function(x,para) {
   if(! are.pargum.valid(para)) return()
   U <- para$para[1]
   A <- para$para[2]

   f <- vector(mode="numeric")
   for(i in seq(1,length(x))) {
     Y <- -(x[i]-U)/A  # condition of GEV(kappa=0)
     f[i] = exp(-exp(Y)) # HW1997, p.195
   }
   return(f)
}

