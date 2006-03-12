"freq.curve.cau" <-
function(fs,para) {
  if(! check.fs(fs)) return()
  if(! is.cau(para)) return()
  Q <- vector(mode="numeric",length = length(fs))
  if(! are.parcau.valid(para)) return(Q)
  for(i in seq(1,length(fs))) {
    Q[i] <- quacau(fs[i],para)
  }
  return(Q)
}

