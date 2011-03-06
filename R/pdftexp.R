"pdftexp" <-
function(x,para) {
    if(! are.partexp.valid(para)) return()

    f <- vector(mode="numeric", length=length(x))

    if(para$is.uni) {
      A <- para$para[1]
      B <- para$para[2]
      my.f <- 1/(B-A)
      for(i in seq(1,length(x))) {
        Y <- x[i]
        if(Y < 0) { f[i] <- 0; next }
        if(Y > U) { f[i] <- 0; next }
        f[i] <- my.f
      }
      return(f)
    }

    U <- para$para[1]
    A <- 1/para$para[2]

    for(i in seq(1,length(x))) {
      Y <- x[i]
      if(Y < 0) { f[i] <- 0; next }
      if(Y > U) { f[i] <- 0; next }
      f[i] <- A*exp(-A*Y)/(1 - exp(-A*U))
    }
    return(f)
}

