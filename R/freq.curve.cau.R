"freq.curve.cau" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.cau(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quacau(fs[i],para)
  }
  return(Q)
}

