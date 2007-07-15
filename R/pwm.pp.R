"pwm.pp" <-
function(x,A=0,B=0,nmom=5,sort=TRUE) {

  if(A <= -1 | A >= B) {
    warnings("Plotting position parameters are invalid")
    return(NULL)
  }

  # To mimic behavior of Hosking's code
  # Default call is to produce unbiased estimates
  if(A == 0 & B == 0) return(pwm(x,nmom=5,sort=TRUE))

  if(sort) x <- sort(x)
  n <- length(x)
  
  alphas <- vector(mode="numeric")
  for(r in seq(0,nmom-1)) {
    i <- r+1
    sum <- 0
    for(j in seq(1,n)) {
      pp <- (j+A)/(n+B) # the plotting position
      sum <- sum + (1-pp)^r*x[j]
    }
    alphas[i] <- sum/n
  }

  betas <- vector(mode="numeric")
  betas[1] <- alphas[1]
  for(r in seq(1,nmom-1)) {
    i <- r+1
    betas[i] <- -1*alphas[i]
  }
  z <- list(betas=betas,
            source="pwm.pp",
            A=A,
            B=B)
  return(z)
}

