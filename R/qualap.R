"qualap" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.parlap.valid(para)) return()
    }
    XI <- para$para[1]
    A  <- para$para[2]

    x <- vector(mode="numeric", length=length(f))
    for(i in seq(1,length(f))) {
      my.f <- f[i]
      if(my.f <= 0.5) {
        x[i] <- XI + A * log(2*my.f)
      } else {
        x[i] <- XI - A * log(2*(1-my.f))
      }
    }
    return(x)
}

