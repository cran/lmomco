"pdfrevgum" <-
function(x,para) {
   if(! are.parrevgum.valid(para)) return()
   U <- para$para[1]
   A <- para$para[2]
   IA <- 1/A
   f <- vector(mode="numeric", length=length(x))
   for(i in seq(1,length(x))) {
     Y <- (x[i]-U)*IA
     f[i] = IA * exp(Y) * ( exp( -exp(Y) ) )
   }
   return(f)
}

