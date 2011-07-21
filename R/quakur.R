"quakur" <-
function(f,para,paracheck=TRUE) {
   if(! check.fs(f)) return()
   if(paracheck == TRUE) {
     if(! are.parkur.valid(para)) return()
   }
   A <- para$para[1] 
   B <- para$para[2] 
   n <- length(f)
   x <- vector(mode="numeric",length=n)
   for(i in seq(1,n)) {
     x[i] <- (1 - (1-f[i])^(1/B))^(1/A)
   }
  return(x)
}
