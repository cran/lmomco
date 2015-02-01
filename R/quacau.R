"quacau" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.parcau.valid(para)) return()
    }
    U <- para$para[1]
    A <- para$para[2]
    x <- vector(mode="numeric", length=length(f))
    for(i in seq(1,length(f))) {
      if(f[i] == 1) { x[i] <- Inf; next }
      if(f[i] == 0) { x[i] <- -Inf; next }
      if(f[i] == 0.5) { x[i] <- U; next }
      x[i] <- U + A*tan(pi*(f[i]-0.5))
    }
    return(x)
}
