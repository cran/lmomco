"cdfgum" <-
function(x,para) { 
   if(! are.pargum.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 

   f <- vector(mode="numeric")
   for(i in seq(1,length(x))) {
     Y <- (x[i]-U)/A 
     f[i] = exp(-exp(-Y)) 
   }
   return(f)
}

