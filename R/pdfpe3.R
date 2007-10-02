"pdfpe3" <- 
function(x,para) {

  # This function is a verbatim implementation of 
  # Pearson Type III CDF as defined by Hosking and Wallis (1997, p. 200)
  # This function represents a complete break from Hosking's FORTRAN 
  # implementation seen in cdfpe3.original().
 
  if(! are.parpe3.valid(para)) return()

  MU    <- para$para[1] # location
  SIGMA <- para$para[2] # scale
  GAMMA <- para$para[3] # shape

  f <- vector(mode="numeric")

  if(GAMMA == 0) { # distribution is normal
    for(i in seq(1,length(x))) {
      f[i] = dnorm((x[i] - MU)/SIGMA)
    }
    return(f)
  }

  # GAMMA != 0, distribution is nonnormal

  # Letting
  ALPHA <- 4/GAMMA^2
  BETA  <- 0.5*SIGMA*abs(GAMMA)
  XI    <- MU - 2*SIGMA/GAMMA
  
  tmp <- gamma(ALPHA)
  if(GAMMA > 0) {
    for(i in seq(1,length(x))) {
      Y <- x[i] - XI
      f[i] = (Y)^(ALPHA - 1) * exp(-Y/BETA) / (BETA^ALPHA * tmp)
    }
    return(f)
  }
  else {
  	for(i in seq(1,length(x))) {
  	  Y <- XI - x[i]
      f[i] = (Y)^(ALPHA - 1) * exp(-Y/BETA) / (BETA^ALPHA * tmp)
    }
    return(f)  	
  }
}
