"pmoms" <-
function(x) {
  n  <- length(x)

  if(n < 4) {
    warning("Four or more (hopefully much more) data values are needed")
    return(NULL) 
  }

  MU <- mean(x)
  SD <- sd(x) # returns with the proper n - 1
              # division for bias correction of variance
  my.term  <- exp(lgamma((n-1)/2) - lgamma(n/2)) # this construct--avoid overflow
  SD.prime <- my.term / sqrt(2)
  SD.prime <- SD.prime * sqrt(sum((x - MU)^2))

  M2 <- sqrt(sum((x-MU)^2) / n) # theoretical definition of standard deviation 

  M3 <- sum((x-MU)^3) / n 
  classic.skew  <- M3 / M2^3 # theoretical definition of skew
  skew <- n^2*M3 / ((n-1)*(n-2)*SD^3) # correct for sample size bias

  M4 <- sum((x-MU)^4) / n 
  classic.kurt  <- M4 / M2^4 # theoretical definition of kurtosis
  kurt <- n^3*M4 / ((n-1)*(n-2)*(n-3)*SD^4) # correct for sample size bias

  # the excess kurtosis term is seen in some software packages
  # including your typical spreadsheet software
  exc <- (3*(n-1)^2)/((n-2)*(n-1))
  kurt <- kurt # - exc
  classic.kurt  <- classic.kurt  # - exc
 
  # For the bias corrections above, I used
  # S.L. Dingman (1992) "Physical Hydrology", Appendix C.
  
  # Because Kurtosis varies so much by definition or structural presentation of the math
  # for verification, lets look at some other definitions.

  # Hosking and Wallis (1997, eq. 2.23) have a slight rearrangement of algebra
  # M2 and M4 are defined in HW(1997, eq. 2.20) 
  #tmp <- n^2 / ((n-2)*(n-3))
  #hosking.k <- (n+1)/(n-1) * M4 - 3*M2^4
  #hosking.k <- hosking.k * tmp
  #hosking.k <- hosking.k/SD^4 + 3
  #cat(c("DEBUG: Hosking's k",hosking.k,"\n"))

  # Stedinger and others, "Handbook of Hydrology" (1992, p. 18.4)
  #HB.k <- (M4 / SD^4) - 3*(n-1)^2/((n-2)*(n-3))
  #cat(c("DEBUG: Stedinger and others:",HB.k,"\n"))

  z <- list(moments = c(MU,SD,M3,M4),
            ratios  = c(NA,SD/MU,skew,kurt),
            sd            = SD,
            umvu.sd       = SD.prime,
            skew          = skew,
            kurt          = kurt,
            classic.sd    = M2,
            classic.skew  = classic.skew,
            classic.kurt  = classic.kurt,
            message = c("The 'classic' values should be slightly different (the bias).",
                        "sd is not truly unbiased; although, surprizingly sd^2 (variance) is."),            
            source = "pmoms")
  return(z)
}
