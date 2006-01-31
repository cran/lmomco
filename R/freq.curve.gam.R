"freq.curve.gam" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gam(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagam(fs[i],para)
  }
  return(Q)
}

