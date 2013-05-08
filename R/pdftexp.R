"pdftexp" <-
function(x,para) {
    if(! are.partexp.valid(para)) return()

    f <- vector(mode="numeric", length=length(x))

    U <- para$para[1]
    B <- 1/para$para[2]
    S <- para$para[3]

    eps <- 1e-6 # one ppm
    if(S) { # stationary
       names(S) <- NULL
       return(rep(1/S, length(x)))
    } else if(is.na(U)) {
       return(dexp(x, rate=B))
    } else {
      BU <- B / (1 - exp(-B*U))
      for(i in seq(1,length(x))) {
         Y <- x[i]
         if(Y <  0) { f[i] <- 0; next }
         if(abs(Y - U) <= eps) { f[i] <- 0; next }
         f[i] <- exp(-B*Y) * BU
      }
      return(f)
    }
}

