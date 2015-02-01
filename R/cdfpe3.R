"cdfpe3" <- 
function(x,para) {

  # This function is a verbatim implementation of 
  # Pearson Type III CDF as defined by Hosking and Wallis (1997, p. 200)
  # This function represents a complete break from Hosking's FORTRAN 
  # implementation seen in cdfpe3.original().
 
  if(! are.parpe3.valid(para)) return()

  MU    <- para$para[1] # location
  SIGMA <- para$para[2] # scale
  GAMMA <- para$para[3] # shape

  f <- vector(mode="numeric", length=length(x))

  if(GAMMA == 0) { # distribution is normal
    for(i in seq(1,length(x))) {
      f[i] = pnorm((x[i] - MU)/SIGMA)
    }
    return(f)
  }

  # GAMMA != 0, distribution is nonnormal

  # Letting
  ALPHA <- 4/GAMMA^2
  BETA  <- 0.5*SIGMA*abs(GAMMA)
  XI    <- MU - 2*SIGMA/GAMMA

  # 'pgamma' is closely related to the incomplete gamma function.  As
  # defined by Abramowitz and Stegun 6.5.1
  #
  #      P(a,x) = 1/Gamma(a) integral_0^x t^(a-1) exp(-t) dt

  # P(a, x) is 'pgamma(x, a)'.  Other authors (for example Karl
  # Pearson in his 1922 tables) omit the normalizing factor, defining
  # the incomplete gamma function as 'pgamma(x, a) * gamma(a)'.
  
  # **** Note the switch in argument order between definition and R's
  # implementation **** This screwed me up at first code version.  


  # HW1997 defines G(ALPHA,x) = integral_0^x t^(a-1) exp(-t) dt
  #      and F(x) = G(ALPHA,(x-XI)/BETA)/COMPLETE_GAMMA(ALPHA) for GAMMA > 0

  # The definition in R permits us to not call gamma(ALPHA) at all
  if(GAMMA > 0) {
  	G <- pgamma((x-XI)/BETA,ALPHA)
    return(G)
  }
  else {
  	G <- pgamma((XI - x)/BETA,ALPHA)  	
    return(1 - G)  # must be GAMMA < 0
  }
}
