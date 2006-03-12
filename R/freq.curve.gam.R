"freq.curve.gam" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.gam(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.pargam.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagam(fs[i],para)
  }
  return(Q)
}

