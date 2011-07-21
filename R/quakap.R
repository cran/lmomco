"quakap" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) { 
      if(! are.parkap.valid(para)) return()
    }
    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]
    n <- length(f)
    x <- vector(mode="numeric",length(f))
    for(i in seq(1,n)) {
      if(f[i] == 0) {
        if(H <= 0 & G  < 0) { x[i] <- U+A/G; next }
        if(H >  0 & G != 0) { x[i] <- U+A/G*(1-H^-G); next }
        if(H >  0 & G == 0) { x[i] <- U+A*log(H); next }
        if(H <= 0 & G >= 0) {
          x[i] <- -Inf
        }
        stop("f is fine: should not be here in code execution.")
      }
      else  if(f[i] == 1) {
        if(G <= 0) {
          x[i] <- Inf
        }
        else {
          x[i] <- U+A/G
          next
        }
        stop("f=1: should not be here in code execution.")
      }
      else {
        Y <- -log(f[i])
        if(H != 0) Y <- (1-exp(-H*Y))/H
        Y <- -log(Y)
        if(G != 0) Y <- (1-exp(-G*Y))/G
        x[i] <- U+A*Y
        next
      }
    }
    return(x)
}

