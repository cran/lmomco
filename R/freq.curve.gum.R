"freq.curve.gum" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.gum(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.pargum.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagum(fs[i],para)
  }
  return(Q)
}

