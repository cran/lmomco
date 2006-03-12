"freq.curve.gld" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.gld(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.pargld.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quagld(fs[i],para,paracheck=FALSE)
  }
  return(Q)
}

