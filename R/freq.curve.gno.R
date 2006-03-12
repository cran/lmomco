"freq.curve.gno" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.gno(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.pargno.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagno(fs[i],para)
  }
  return(Q)
}

