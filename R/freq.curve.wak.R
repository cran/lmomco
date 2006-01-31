"freq.curve.wak" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.wak(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quawak(fs[i],para)
  }
  return(Q)
}

