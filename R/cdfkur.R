"cdfkur" <-
function(x,para) {
   if(! are.parkur.valid(para)) return()
   A <- para$para[1]
   B <- para$para[2]

   f <- vector(mode="numeric", length=length(x))
   for(i in seq(1,length(x))) {
     if(x[i] >= 0 & x[i] <= 1) {
       f[i] = 1 - (1 - x[i]^A)^B
     } else {
       f[i] <- NA;
     }
   }
   return(f)
}
