"freq.curve.gpa" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.gpa(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.pargpa.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagpa(fs[i],para)
  }
  return(Q)
}

