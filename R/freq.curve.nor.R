"freq.curve.nor" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.nor(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.parnor.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quanor(fs[i],para)
  }
  return(Q)
}

