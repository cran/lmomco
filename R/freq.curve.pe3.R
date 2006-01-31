"freq.curve.pe3" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.pe3(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quape3(fs[i],para)
  }
  return(Q)
}

