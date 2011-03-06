"lmomtexp" <-
function(para) {
    z <- list(L1   = NULL,
              L2   = NULL,
              TAU3 = NULL,
              TAU4 = NULL,
              TAU5 = NULL,
              LCV  = NULL,
              L3   = NULL,
              L4   = NULL,
              L5   = NULL,
              source = "lmomtexp"
             )
    if(! are.partexp.valid(para)) return()
    attributes(para$para) <- NULL

    U <- para$para[1]
    A <- 1/para$para[2]

    if(para$is.uni) {
      z$L1   <- (para$para[2] + para$para[1])/2
      z$L2   <- (para$para[2] - para$para[1])/6
      z$LCV  <- z$L2/z$L1
      z$L3   <- 0
      z$L4   <- 0
      z$TAU3 <- 0
      z$TAU4 <- 0
      return(z)
    }

    ETA <- exp(-A*U)
    ETA1 <- (1-ETA)

    z$L1   <- 1/A - U*ETA/ETA1
    z$L2   <- 1/ETA1 * (((1+ETA)/(2*A)) - (U*ETA/ETA1))
    z$LCV  <- z$L2/z$L1

    z$L3   <- 1/ETA1^2 * ( ( (1+10*ETA+ETA^2) / (6*A) ) -
                           ( (U*ETA*(1+ETA))  / ETA1)
                         )
    z$L4   <- 1/ETA1^3 * ( ( (1+29*ETA+29*ETA^2+ETA^3) / (12*A) ) -
                           ( (U*ETA*(1+3*ETA+ETA^2))   / ETA1)
                         )

    z$TAU3 <- z$L3 / z$L2
    z$TAU4 <- z$L4 / z$L2
    return(z)
}

