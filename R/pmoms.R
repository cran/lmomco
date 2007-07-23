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

  M2 <- sqrt(sum((x-MU)^2) / n) # theoretical definition of standard deviation 

  M3 <- sum((x-MU)^3) / n 
  classic.skew  <- M3 / M2^3 # theoretical definition of skew
  unbiased.skew <- n^2*M3 / ((n-1)*(n-2)*SD^3) # correct for sample size bias

  M4 <- sum((x-MU)^4) / n 
  classic.kurt  <- M4 / M2^4 # theoretical definition of kurtosis
  unbiased.kurt <- n^3*M4 / ((n-1)*(n-2)*(n-3)*SD^4) # correct for sample size bias

  # the excess kurtosis term is seen in some software packages
  # including your typical spreadsheet software
  exc <- (3*(n-1)^2)/((n-2)*(n-1))
  unbiased.kurt <- unbiased.kurt - exc
  classic.kurt  <- classic.kurt  - exc
 
  # For the bias corrections above, I used
  # S.L. Dingman (1992) "Physical Hydrology", Appendix C.
  
  # Because Kurtosis varies so much by definition or structural presentation of the math
  # for verification, lets look at some other definitions.

  # Hosking and Wallis (1997, eq. 2.23) have a slight rearrangement of algebra
  # M2 and M4 are defined in HW(1997, eq. 2.20) 
  #tmp <- n^2 / ((n-2)*(n-3))
  #hosking.k <- (n+1)/(n-1) * M4 - 3*M2^4
  #hosking.k <- hosking.k * tmp
  #hosking.k <- hosking.k/SD^4 # note that I don't have the addition of 3 like hosking as I want excess kurtosis
  #cat(c("DEBUG: Hosking's k",hosking.k,"\n"))

  # Stedinger and others, "Handbook of Hydrology" (1992, p. 18.4)
  #HB.k <- (M4 / SD^4) - 3*(n-1)^2/((n-2)*(n-3))
  #cat(c("DEBUG: Stedinger and others:",HB.k,"\n"))

  z <- list(moments = c(MU,SD,M3,M4),
            ratios  = c(NA,SD/MU,unbiased.skew,unbiased.kurt),
            unbiased.sd   = SD,
            unbiased.skew = unbiased.skew,
            unbiased.kurt = unbiased.kurt,
            classic.sd    = M2,
            classic.skew  = classic.skew,
            classic.kurt  = classic.kurt,
            message = c("The kurtosis values are excess kurtosis from the normal distribution, which has a kurtosis of 3.",
                        "The 'classic' values should be slightly different (the bias).",
                        "unbiased.sd is not truly unbiased; although, surprizingly unbiased.sd^2 (variance) is."),            
            source = "pmoms")
  return(z)
}
