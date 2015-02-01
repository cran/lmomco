"pdfgum" <-
function(x,para) { 
   if(! are.pargum.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 

   f <- vector(mode="numeric", length=length(x))
   for(i in seq(1,length(x))) {
     Y <- -(x[i]-U)/A 
     f[i] = A^(-1)*exp(Y)*exp(-exp(Y)) 
   }
   return(f)
}

