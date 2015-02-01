"cdfsla" <-
function(x, para) {
    EPS <- sqrt(.Machine$double.eps) 
    SMALL <- 1E-6 # hacked into limit based on behavior of dnorm()

    if(! are.parsla.valid(para)) return()

    U <- para$para[1]
    A <- para$para[2]
    tmp <- dnorm(0)
    
    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      if(x[i] == -Inf) { f[i] <- 0; next }
      if(x[i] ==  Inf) { f[i] <- 1; next }
      Y <- (x[i] - U)/A
      the.diff <- tmp - dnorm(Y)
      if(abs(tmp) <= EPS | Y == 0) {
         f[i] <- 0.5
      } else if(the.diff == 0 & abs(Y) < SMALL) { # another trap for discontinuity
         f[i] <- 0.5
      }
      else {
         f[i] <- pnorm(Y) - the.diff/Y
      }
    }
    return(f)
}

