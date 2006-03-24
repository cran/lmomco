"theoLmoms" <- 
function(para,nmom=5,verbose=FALSE) {
  if(nmom < 1) {
    warning("Number of L-moments requested is less than 1")
    return()
  }
  TL <- theoTLmoms(para,nmom=nmom,trim=0,verbose=verbose)
  z <- list(lambdas = TL$lambdas, ratios = TL$ratios, trim=0,
            source="theoLmoms")
  return(z)
}
