"freq.curve.wei" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.wei(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.parwei.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quawei(fs[i],para)
  }
  return(Q)
}

