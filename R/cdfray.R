"cdfray" <-
function(x,para) { 
   if(! are.parray.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 

   f <- vector(mode="numeric")
   for(i in seq(1,length(x))) {
     f[i] = 1 - exp(-1*(x[i] - U)^2/(2*A^2))
   }
   return(f)
}

