"cdfaep" <-
function(x, para, paracheck=TRUE) {

    if(paracheck == TRUE) {
      if(! are.paraep.valid(para)) return()
    }

    U <- para$para[1]
    A <- para$para[2]
    K <- para$para[3]
    H <- para$para[4]

    # The "a" argument to the incomplete gamma functions
    IGA <- 1/H
    # There is no need for gamma(a) as shown in the literature
    # because R's pgamma() includes 1/gamma(a) in the computation

    AK1 <- (A*K)^-H
    AK2 <- (K/A)^H

    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
       myx <- x[i]
       if(myx < U) {
          IGB <- AK1 * (U - myx)^H
          Qax <- pgamma(IGB, IGA, lower.tail=FALSE, log.p=TRUE)
          f[i] <- K^2/(1+K^2) * exp(Qax)
       } else {
          IGB <- AK2 * (myx - U)^H
          Qax <- pgamma(IGB, IGA, lower.tail=FALSE, log.p=TRUE)
          f[i] <- 1 - 1/(1+K^2) * exp(Qax)
       }
   }
  return(f)
}

# 'pgamma' is closely related to the incomplete gamma function.  As
  # defined by Abramowitz and Stegun 6.5.1
  #
  #      P(a,x) = 1/Gamma(a) integral_0^x t^(a-1) exp(-t) dt

  # P(a, x) is 'pgamma(x, a)'.  Other authors (for example Karl
  # Pearson in his 1922 tables) omit the normalizing factor, defining
  # the incomplete gamma function as 'pgamma(x, a) * gamma(a)'.
  # A few use the ‘upper’ incomplete gamma function, the integral from x
  # to infinity which can be computed by pgamma(x, a, lower=FALSE) * gamma(a),
  # for its normalized version.
  # **** Note the switch in argument order between definition and R's
  # implementation **** This screwed me up at first code version.


  # HW1997 defines G(ALPHA,x) = integral_0^x t^(a-1) exp(-t) dt
  #      and F(x) = G(ALPHA,(x-XI)/BETA)/COMPLETE_GAMMA(ALPHA) for GAMMA > 0

# GNU: These routines compute the normalized incomplete Gamma Function
# Q(a,x) = 1/\Gamma(a) \int_x^\infty dt t^{a-1} \exp(-t) for a > 0, x >= 0.
# Note in the implementation of cdfaep the use of lower.tail=FALSE)

