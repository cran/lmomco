"freq.curve.kap" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.kap(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.parkap.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quakap(fs[i],para)
  }
  return(Q)
}

