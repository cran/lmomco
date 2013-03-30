"quatexp" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.partexp.valid(para)) return()
    }

    n <- length(f)
    x <- vector(mode="numeric",length=n)

    U <- para$para[1]
    B <- 1/para$para[2]
    S <- para$para[3]

    if(S) { # stationary
       for(i in seq(1,n)) {
         x[i] <- S*f[i]
       }
       return(x)
    } else if(is.na(U)) {
       return(qexp(f, rate=B))
    } else {
       BU <- 1 - exp(-B*U)
       for(i in seq(1,n)) {
         x[i] <- - log(1 - f[i]*BU) / B
       }
       x[! is.finite(x)] <- - log(.Machine$double.eps) / B
       return(x)
    }
}

