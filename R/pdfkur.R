"pdfkur" <-
function(x,para) { 
   if(! are.parkur.valid(para)) return()
   A <- para$para[1] 
   B <- para$para[2] 

   f <- vector(mode="numeric")
   for(i in seq(1,length(x))) {
     if(x[i] > 0 & x[i] < 1) {
       f[i] = A*B*x[i]^(A-1)*(1-x^A)^(B-1)
     } else {
       f[i] <- NA;
     }
   }
   return(f)
}

