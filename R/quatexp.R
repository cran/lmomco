"quatexp" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.partexp.valid(para)) return()
    }
    
    n <- length(f)
    x <- vector(mode="numeric",length=n)

    if(para$is.uni) {
      A <- para$para[1]
      B <- para$para[2]
      D <- (B-A)
      for(i in seq(1,n)) {
        x[i] <- A + D*f[i]
      }
      return(x)
    }

    U <- para$para[1]
    A <- 1/para$para[2]
    for(i in seq(1,n)) {
      x[i] <- - log(1 - f[i]*(1-exp(-A*U))) / A
    }
    return(x)
}

