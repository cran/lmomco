"freq.curve.gld" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gld(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagld(fs[i],para)
  }
  return(Q)
}

