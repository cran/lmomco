"TLmoms" <-
function(x,nmom=5,trim=0) {
  if(nmom < 1) {
    warning("Number of L-moments is less than 1")
    return(FALSE)
  }
  if(trim < 0) {
    warning("Trimming value is less than 0")
    return(FALSE)
  }
  t <- trim
  x <- sort(x)
  n <- length(x)
  L <- seq(1,nmom)
  R <- seq(1,nmom)
  for(r in seq(1,nmom)) {
    lambda <- TLmom(x,trim=trim,order=r,sort=FALSE)
    lr <- lambda$lambda
    L[r] <- lr
  }
  L
  if(nmom >= 2) {
    R[2] <- L[2]/L[1]
  }
  if(nmom >= 3) {
    for(r in seq(3,nmom)) {
      R[r] <- L[r]/L[2]
    }
  }
  R[1] <- 0
  z <- list(lambdas = L, ratios = R, trim=trim, source="TLmoms")
  return(z)
}
