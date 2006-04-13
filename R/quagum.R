"quagum" <-
function(f,para) {
   if(! are.pargum.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 

   x <- vector(mode="numeric")
   for(i in seq(1,length(f))) {
     if(f[i] <= 0 || f[i] >= 1) {
       warning("nonexceedance probability value invalid")
       return()
     }
     x[i] <- U-A*log(-log(f[i]))
   }
  return(x)
}

