"lmomwak" <-
function(wakpara) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL
             )

    if(! are.parwak.valid(wakpara)) return()

    XI <- wakpara$para[1]
    A  <- wakpara$para[2]
    B  <- wakpara$para[3]
    C  <- wakpara$para[4]
    D  <- wakpara$para[5]

    #  LAMBDA-1
    #
    Y <- A/(1+B)
    Z <- C/(1-D)
    z$L1 <- XI+Y+Z

    #  LAMBDA-2
    #
    Y <- Y/(2+B)
    Z <- Z/(2-D)
    ALAM2 <- Y+Z
    z$L2 <- ALAM2

    #  HIGHER MOMENTS
    #
    x <- matrix(nrow = 5, ncol = 1)
    for(M in seq(3,5)) {
      Y <- Y*(M-2-B)/(M+B)
      Z <- Z*(M-2+D)/(M-D)
      x[M] <- (Y+Z)/ALAM2
    }
    z$TAU3 <- x[3]
    z$TAU4 <- x[4]
    z$TAU5 <- x[5]
    z$LCV  <- z$L2/z$L1
    z$L3   <- z$TAU3*z$L2
    z$L4   <- z$TAU4*z$L2
    z$L5   <- z$TAU5*z$L2
    return(z)
}

