"quape3" <-
function(f,para) { 
    if(! are.parpe3.valid(para)) return()

    # SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
    SMALL <- 1e-6

    U <- para$para[1]
    A <- para$para[2]
    GAMMA <- para$para[3]
    if(f <= 0 || f >= 1) {
      if((f == 0 & GAMMA > 0) |
         (f == 1 & GAMMA < 0)) {
        U-2*A/GAMMA
      }
      else {
        warning("Argument to function invalid")
        return()
      }
    }
    if(abs(GAMMA) < SMALL) {
      # ZERO SKEWNESS, qnorm() is the standard normal distribution
      return(U+A*qnorm(f))
    }
    else {
       ALPHA <- 4/GAMMA^2
       BETA <- abs(0.5*A*GAMMA)
       if(GAMMA > 0) {
         return(U-ALPHA*BETA+qgamma(f,ALPHA,scale=BETA))
       }
       else {
         return(U+ALPHA*BETA-qgamma(1-f,ALPHA,scale=BETA))
       }
    }
}

