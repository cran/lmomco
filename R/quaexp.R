"quaexp" <-
function(f,para) {
    if(! are.parexp.valid(para)) return()
    U <- para$para[1]
    A <- para$para[2]

    x <- vector(mode="numeric")
    for(i in seq(1,length(f))) {
      if(f[i] <= 0 || f[i] >= 1) {
        warning("Nonexceedance probability is invalid")
        return()
      }
      x[i] <- U-A*log(1-f[i])
    }
    return(x)
}

