"freq.curve.pe3" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.pe3(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.parpe3.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quape3(fs[i],para)
  }
  return(Q)
}

