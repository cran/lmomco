"pdfray" <-
function(x,para) { 
   if(! are.parray.valid(para)) return()
   U <- para$para[1] 
   A <- para$para[2] 

   AS <- A^2
   f <- vector(mode="numeric", length=length(x))
   for(i in seq(1,length(x))) {
     tmp <- x[i] - U
     f[i] = tmp/AS * exp(-(tmp^2/(2*AS)))
   }
   return(f)
}

