"quagum" <-
function(f,para,paracheck=TRUE) {
   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     if(! are.pargum.valid(para)) return()
   }
   U <- para$para[1] 
   A <- para$para[2] 
   x <- vector(mode="numeric", length=length(f))
   for(i in seq(1,length(f))) {
     x[i] <- U-A*log(-log(f[i]))
   }
  return(x)
}

