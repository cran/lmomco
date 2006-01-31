"freq.curve.nor" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.nor(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quanor(fs[i],para)
  }
  return(Q)
}

