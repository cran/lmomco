"INT.kapicase3" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    # OFL SHOULD BE CHOSEN SO THAT EXP(OFL) JUST DOES NOT CAUSE OVERFLOW
    OFL <- log(.Machine$double.xmax)
    DLGAM <- lgamma(1+G)
    #
    #         - CASE H>0, G NONZERO
    #
    for(R in seq(1,5)) {
      ARG <- DLGAM+lgamma(1+R/H)-lgamma(1+G+R/H)-G*log(H)
      if(abs(ARG) > OFL) {
        warning("Calculations of L-moments have broken down")
        return()
      }
      BETA[R] <- exp(ARG)
    }
    return(BETA)
}

