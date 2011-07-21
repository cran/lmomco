"pwm.pp" <-
function(x, pp=NULL, A=0, B=0, nmom=5, sort=TRUE) {

  n <- length(x)
  
  pp.was.null <- FALSE;
  if(is.null(pp)) {
    pp.was.null <- TRUE;
    # To mimic behavior of Hosking's code
    # Default call is to produce unbiased estimates
    if(A == 0 & B == 0) {
      z <- pwm(x, nmom=nmom, sort=sort)
      return(z)
    }
    
    if(A <= -1 | A >= B) {
      warnings("Plotting position parameters are invalid")
      return(NULL)
    }
    
    pp <- ((1:n) + A/(n + B))
  }
  
  if(length(x) != length(pp)) {
     warnings("Vectors 'x' and plotting positions 'pp' are unequal in length");
     return(NULL)
  }
  
  if(sort) {
    ix <- sort(x, index.return=TRUE)$ix
     x <- x[ix]
    pp <- pp[ix]
  }
  
  
  betas <- vector(mode="numeric", length=n)
  for(r in seq(0,nmom-1)) {
    tmp <- sapply(1:n, function(j) { return(pp^r*x[j]) })
    betas[r+1] <- sum(tmp)/n
  }
  
  z <- list(betas=betas, source="pwm.pp")
  if(pp.was.null) {
    z$A <- A
    z$B <- B
  } else {
    z$F <- list(F=pp)
  }
  return(z)
}

