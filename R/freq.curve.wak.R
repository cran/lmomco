"freq.curve.wak" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.wak(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.parwak.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quawak(fs[i],para)
  }
  return(Q)
}

