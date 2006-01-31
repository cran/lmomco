"INT.kapicase2" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    DLGAM <- lgamma(1+G)
    #
    #         - CASE H SMALL, G NONZERO
    #
    for(R in seq(1,5)) {
      BETA[R] <- exp(DLGAM-G*log(R))*(1-0.5*H*G*(1+G)/R)
    }
    return(BETA)
}

