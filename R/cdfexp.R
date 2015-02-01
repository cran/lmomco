"cdfexp" <-
function(x,para) {
    if(! are.parexp.valid(para)) return()
    U <- para$para[1]
    A <- para$para[2]

    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      Y <- (x[i]-U)/A
      if(Y <= 0) { f[i] <- 0; next }
      f[i] <- (1-exp(-Y))
    }
    return(f)
}

