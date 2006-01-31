"freq.curve.gpa" <-
function(fs,para) {
  if(! INT.check.fs(fs)) return()
  if(! is.gpa(para)) return()
  Q <- matrix(nrow = length(fs), ncol = 1)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagpa(fs[i],para)
  }
  return(Q)
}

