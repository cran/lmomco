"pp" <-
function(x,A=0,B=1) {
    if(A <= -1 | A >= B) {
      warning("Plotting position parameters are invalid")
      return()
    }
    N <- length(x)
    pp <- vector(mode = "numeric")

    for(i in seq(1,N)) {
      pp[i] <- (i+A)/(N+B)
    }
    return(pp)
}

