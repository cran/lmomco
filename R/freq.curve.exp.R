"freq.curve.exp" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.exp(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quaexp(fs[i],para)
  }
  return(Q)
}

