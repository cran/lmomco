"quarice" <-
function(f, para, xmax=NULL) {
   if(! are.parrice.valid(para)) return()
   V   <- para$para[1]
   A   <- V/para$para[2]
   names(A) <- "Alpha"
   if(is.null(xmax)) {
      for(ord in (1:10)) {
        test.xmax <- 10^ord*(V+A)
        val <- pdfrice(test.xmax, para)
        if(val <= 100*.Machine$double.eps) {
          xmax <- test.xmax
          break
        }
        ifelse(is.finite(val), xmax <- test.xmax, break)
        ord <- ord + 1
      }
   }
   x <- vector(mode="numeric")
   for(i in 1:length(f)) {
     Fx   <- f[i]
     x[i] <- NA
     if(Fx < 0 | Fx >= 1) {
       warning("invalid nonexceedance probability")
       next
     }
     if(Fx == 0) {
       x[i] <- 0
     } else if(Fx == 1) {
       x[i] <- Inf # is this ok?
     } else {
       try( x[i] <-
         uniroot(function(X,...)
                  return(Fx - cdfrice(X,...)),
                  c(0,xmax), para=para)$root, silent=FALSE)
     }
   }
   return(x)
}


#"dorice" <- function(rF,a,v) {
#  F <- nonexceeds()
#  rice <- vec2par(c(a,v), type='rice')
#  plot(F, quarice(F,rice), xlim=c(0,1-(1-rF)/10))
#  lines(c(rF,rF),
#        c(quarice(rF, rice),
#          quarice(rF, rice)))
#}
