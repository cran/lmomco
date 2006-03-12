"freq.curve.exp" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.exp(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.parexp.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quaexp(fs[i],para)
  }
  return(Q)
}

