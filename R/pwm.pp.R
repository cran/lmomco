"pwm.pp" <-
function(x,A,B) {
    N <- length(x)
    #  FOR UNBIASED ESTIMATES SET A AND B EQUAL TO ZERO. OTHERWISE,
    #  PLOTTING-POSITION ESTIMATORS ARE USED, BASED ON THE PLOTTING POSITION
    #  (J+A)/(N+B)  FOR THE J'TH SMALLEST OF N OBSERVATIONS. FOR EXAMPLE,
    #  A=-0.35D0 AND B=0.0D0 YIELDS THE ESTIMATORS RECOMMENDED BY
    #  HOSKING ET AL. (1985, TECHNOMETRICS) FOR THE GEV DISTRIBUTION.
    #
    PWM <- vector(mode="numeric",length=5)
    for(j in seq(1,5)) PWM[j] <- 0

    if(A == 0 & B == 0) {
      # UNBIASED ESTIMATES OF PWM'S
      for(i in seq(1,N)) {
        WEIGHT <- 1/N
        PWM[1] <- PWM[1] + WEIGHT*x[i]
        for(j in seq(2,5)) { 
          jm <- j-1
          WEIGHT <- WEIGHT*(i-jm)/(N-jm)
          PWM[j] <- PWM[j]+WEIGHT*x[i]
        }
      }
      z <- list(BETA0 = PWM[1], BETA1 = PWM[2], BETA2 = PWM[3],
                BETA3 = PWM[4], BETA4 = PWM[5])
      return(z)
    }
    if(A <= -1 | A >= B) {
      warning("Plotting position parameters are invalid.")
      return()
    }
    #
    # PLOTTING-POSITION ESTIMATES OF PWM'S
    #
    for(i in seq(1,N)) {
      PPOS <- (i+A)/(N+B)
      TERM <- x[i]
      PWM[1] <- PWM[1]+TERM
      for(j in seq(2,5)) {
        TERM <- TERM*PPOS
        PWM[j] <- PWM[j]+TERM
      }
    }
    for(j in seq(1,5)) PWM[j] <- PWM[j]/N
    z <- list(BETA0 = PWM[1], BETA1 = PWM[2], BETA2 = PWM[3],
              BETA3 = PWM[4], BETA4 = PWM[5])
    return(z)
}

