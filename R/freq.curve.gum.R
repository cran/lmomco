"freq.curve.gum" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gum(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagum(fs[i],para)
  }
  return(Q)
}

