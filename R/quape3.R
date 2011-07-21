"quape3" <-
function(f,para,paracheck=TRUE) { 
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.parpe3.valid(para)) return()
    }
    # SMALL IS USED TO TEST WHETHER SKEWNESS IS EFFECTIVELY ZERO
    SMALL <- 1e-6

    U <- para$para[1]
    A <- para$para[2]
    GAMMA <- para$para[3]
    n <- length(f)
    x <- vector(mode="numeric",length=n)
    for(i in seq(1,n)) {
      if((f[i] == 0 && GAMMA > 0) ||
         (f[i] == 1 && GAMMA < 0)) {
        x[i] <- U-2*A/GAMMA
        next
      }
      if(abs(GAMMA) < SMALL) {
        # ZERO SKEWNESS, qnorm() is the standard normal distribution
        x[i] <- U+A*qnorm(f[i])
        next
      }
      else {
         ALPHA <- 4/GAMMA^2
         BETA <- abs(0.5*A*GAMMA)
         if(GAMMA > 0) {
           x[i] <- U-ALPHA*BETA+qgamma(f[i],ALPHA,scale=BETA)
         }
         else {
           x[i] <- U+ALPHA*BETA-qgamma(1-f[i],ALPHA,scale=BETA)
         }
      }
    }
    return(x)
}

