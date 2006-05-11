"pp" <-
function(x,a=0) {
    if(a < 0) {
      warning("Plotting position parameter is invalid")
      return()
    }
    N <- length(x)
    pp <- vector(mode = "numeric")

    for(i in seq(1,N)) {
      pp[i] <- (i-a)/(N+1-2*a)
    }
    return(pp)
}

