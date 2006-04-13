"cdfpe3" <-
function(x,para) {
    if(! are.parpe3.valid(para)) return()

    ROOT0p5 <- sqrt(1/2)

    # Error function as defined by R documentation
    #   and is used for zero skew condition
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

    # SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
    SMALL <- 1e-6

    U <- para$para[1]
    A <- para$para[2]
    GAMMA <- para$para[3]

    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
      if(abs(GAMMA) <= SMALL) { 
        # ZERO SKEWNESS
        Z <- (x[i]-U)/A
        f[i] <- 0.5+0.5*erf(Z*ROOT0p5)
        next
      }
      ALPHA <- 4/GAMMA^2
      Z <- 2*(x[i]-U)/(A*GAMMA)+ALPHA
      CDFPE3 <- 0
      if(Z     > 0)  CDFPE3 <- pgamma(Z,ALPHA)
      if(GAMMA < 0 ) CDFPE3 <- 1-CDFPE3
      f[i] <- CDFPE3
    }
    return(f)
}

