"lmomkap" <-
function(para) {
    if(! are.parkap.valid(para)) return()
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
    BETA <- matrix(nrow = 5, ncol = 1)

    # SMALL IS USED TO TEST WHETHER H IS EFFECTIVELY ZERO
    SMALL <- 1e-8

    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]


    #   CALCULATE FUNCTIONS OCCURRING IN THE PWM'S BETA-SUB-R
    ICASE <- 1
    if(H > 0)          ICASE <- 3
    if(abs(H) < SMALL) ICASE <- 2
    if(G == 0)         ICASE <- ICASE+3
    if(ICASE == 1) BETA <- INT.kapicase1(U,A,G,H)
    if(ICASE == 2) BETA <- INT.kapicase2(U,A,G,H)
    if(ICASE == 3) BETA <- INT.kapicase3(U,A,G,H)
    if(ICASE == 4) BETA <- INT.kapicase4(U,A,G,H)
    if(ICASE == 5) BETA <- INT.kapicase5(U,A,G,H)
    if(ICASE == 6) BETA <- INT.kapicase6(U,A,G,H)

    #         LAMBDA-1
    if(G == 0) {
      z$L1 <- U+A*BETA[1]
    }
    else {
      z$L1 <- U+A*(1-BETA[1])/G
    }

    #         LAMBDA-2
    ALAM2 <- BETA[2]-BETA[1]
    if(G == 0) {
      z$L2 <- A*ALAM2
    }
    else {
      z$L2 <- A*ALAM2/(-G)
    }
    z$LCV <- z$L2 / z$L1
    #         HIGHER MOMENTS
    Z0 <- 1
    x <- matrix(nrow = 5, ncol = 1)
    for(J in seq(3,5)) {
      Z0 <- Z0*(4*J-6)/J
      Z <- Z0*3*(J-1)/(J+1)
      SUM <- Z0*(BETA[J]-BETA[1])/ALAM2-Z
      if(J == 3) {
        x[J] <- SUM
      }
      else {
        for(I in seq(2,J-2)) {
          Z <- Z*(I+I+1)*(J-I)/((I+I-1)*(J+I))
          SUM <- SUM-Z*x[I+1]
        }
        x[J] <- SUM
      }
    }
    z$TAU3 <- x[3]
    z$TAU4 <- x[4]
    z$TAU5 <- x[5]
    z$L3   <- z$TAU3*z$LCV
    z$L4   <- z$TAU4*z$LCV
    z$L5   <- z$TAU5*z$LCV
    return(z)
}

