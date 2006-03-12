"freq.curve.gev" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.gev(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.pargev.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagev(fs[i],para)
  }
  return(Q)
}

