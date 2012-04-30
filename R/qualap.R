"qualap" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.parlap.valid(para)) return()
    }
    XI <- para$para[1]
    A  <- para$para[2]

    n <- length(f)
    x <- vector(mode="numeric",length=n)
    for(i in seq(1,n)) {
      my.f <- f[i]
      if(my.f <= 0.5) {
        x[i] <- XI + A * log(2*my.f)
      } else {
        x[i] <- XI - A * log(2*(1-my.f))
      }
    }
    return(x)
}

