"quarevgum" <-
function(f,para,paracheck=TRUE) {
   if(! check.fs(f)) return()
   if(paracheck == TRUE) {   
     if(! are.parrevgum.valid(para)) return()
   }
   U <- para$para[1] 
   A <- para$para[2] 
   n <- length(f)
   x <- vector(mode="numeric",length=n)
   for(i in seq(1,n)) {
     x[i] <- U + A*log(-log(1-f[i]))
   }
  return(x)
}

