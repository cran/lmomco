"cdftexp" <-
function(x,para) {
    if(! are.partexp.valid(para)) return()
    f <- vector(mode="numeric", length=length(x))

    U <- para$para[1]
    B <- 1/para$para[2]
    S <- para$para[3]

    if(S) {
      for(i in seq(1,length(x))) {
        Y <- x[i]
        if(Y < 0) { f[i] <- 0; next }
        if(Y > S) { f[i] <- 1; next }
        f[i] <- Y/S
      }
      return(f)
    } else if(is.na(U)) {
       return(pexp(x, rate=B))
    } else {
       BU <- 1/(1 - exp(-B*U))
       for(i in seq(1,length(x))) {
         Y <- x[i]
         if(Y < 0) { f[i] <- 0; next }
         if(Y > U) { f[i] <- 1; next }
         f[i] <- BU * (1-exp(-B*Y))
       }
       return(f)
    }
}

