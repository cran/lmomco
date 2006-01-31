"freq.curve.glo" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.glo(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quaglo(fs[i],para)
  }
  return(Q)
}

