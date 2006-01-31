"freq.curve.kap" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.kap(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quakap(fs[i],para)
  }
  return(Q)
}

