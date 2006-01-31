"INT.kapicase4" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    #
    #         - CASE H<0, G <- 0
    #
    #   EU  IS EULER'S CONSTANT
    EU <- 0.577215664901532861
    for(R in seq(1,5)) {
      BETA[R] <- EU + log(-H)+digamma(-R/H)
    }
    return(BETA)
}

