"cdftexp" <-
function(x,para) {
    if(! are.partexp.valid(para)) return()

    f <- vector(mode="numeric", length=length(x))

    U <- para$para[1]
    A <- 1/para$para[2]
    if(para$is.uni) {
      A <- para$para[1]
      B <- para$para[2]
      D <- (B-A)
      for(i in seq(1,length(x))) {
        Y <- x[i]
        if(Y < 0) { f[i] <- 0; next }
        if(Y > U) { f[i] <- 1; next }
        f[i] <- (Y - A)/D
      }
      return(f)
    }

    for(i in seq(1,length(x))) {
      Y <- x[i]
      if(Y < 0) { f[i] <- 0; next }
      if(Y > U) { f[i] <- 1; next }
      f[i] <- (1-exp(-A*Y))/(1-exp(-A*U))
    }
    return(f)
}

