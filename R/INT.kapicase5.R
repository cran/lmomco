"INT.kapicase5" <-
function(U,A,G,H) {
    BETA <- matrix(nrow = 5, ncol = 1)
    #
    #         - CASE H SMALL, G <- 0
    #
    #   EU  IS EULER'S CONSTANT
    EU <- 0.577215664901532861
    for(R in seq(1,5)) {
      BETA[R] <- EU+log(R)
    }
    return(BETA)
}

