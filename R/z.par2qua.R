"z.par2qua" <-
function(f,p,para,z=0,...) {
  x <- vector(mode = "numeric", length=length(f))
  for(i in seq(1,length(f))) {
    if(f[i] <= p) {
      x[i] <- z
      next
    }
    nF <- (f[i] - p)/(1 - p)
    nx <- qlmomco(nF,para,...)
    if(nx < z) {
      x[i] <- z
      next
    }
    x[i] <- nx
  }
  return(x)
}
