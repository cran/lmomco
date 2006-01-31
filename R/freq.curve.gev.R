"freq.curve.gev" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gev(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagev(fs[i],para)
  }
  return(Q)
}

