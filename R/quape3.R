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
    x <- vector(mode="numeric", length=length(f))
    for(i in seq(1,length(f))) {
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

