"quaray" <-
function(f,para,paracheck=TRUE) {
   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     if(! are.parray.valid(para)) return()
   }
   U <- para$para[1] 
   A <- para$para[2] 
   x <- vector(mode="numeric", length=length(f))
   for(i in seq(1,length(f))) {
     x[i] <- U + sqrt(-2*A^2 * log(1-f[i]))
   }
  return(x)
}

