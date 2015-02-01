"z.par2cdf" <-
function(x,p,para,z=0,...) {
  f <- vector(mode = "numeric", length=length(x))
  for(i in seq(1,length(x))) {
    if(x[i] < z) {
      f[i] <- 0
      next
    }
    nF <- plmomco(x[i],para,...)
    nF <- p + (1-p)*nF
    f[i] <- nF
  }
  return(f)
}
