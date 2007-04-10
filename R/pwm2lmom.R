"pwm2lmom" <-
function(pwm) {
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
    z$L1 <- pwm$BETA0
    z$L2 <- 2*pwm$BETA1 - pwm$BETA0
    z$L3 <- 6*pwm$BETA2 - 6*pwm$BETA1 + pwm$BETA0
    z$L4 <- 20*pwm$BETA3 - 30*pwm$BETA2 + 12*pwm$BETA1 - pwm$BETA0
    z$L5 <- 70*pwm$BETA4 - 140*pwm$BETA3 + 90*pwm$BETA2 - 20*pwm$BETA1 + pwm$BETA0
    z$LCV <- z$L2/z$L1
    z$TAU3 <- z$L3/z$L2
    z$TAU4 <- z$L4/z$L2
    z$TAU5 <- z$L5/z$L2
    return(z) 
}

