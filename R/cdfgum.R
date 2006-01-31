"cdfgum" <-
function(x,para) { 
   if(! are.pargum.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 
   Y <- (x-U)/A 
   return(exp(-exp(-Y))) 
}

