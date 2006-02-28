"TLmom" <-
function(x,order=1,trim=0,sortdata=TRUE) {
  r <- order
  t <- trim
  if(sortdata == TRUE) x <- sort(x)
  n <- length(x)
  lambda <- 0
  for(i in seq(t+1,n-t)) {
    wk <- 0
    for(k in seq(0,r-1)) {
      term <- (-1)^k * choose(r-1,k) * choose(i-1,r+t-1-k) * choose(n-i,t+k)
      wk <- wk + term
    }
    wk <- wk / choose(n,r+2*t)
    lambda <- lambda + wk * x[i]
  }
  lambda <- lambda/r

  z <- list(lambda = lambda, trim = t, order = r)
  return(z)
}

