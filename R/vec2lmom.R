"vec2lmom" <-
function(vec,lscale=TRUE) {
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
    z$L1 <- vec[1]
    if(lscale == TRUE) {
      z$L2   <- vec[2]
      z$TAU3 <- vec[3]
      z$TAU4 <- vec[4]
      z$TAU5 <- vec[5]

      z$LCV  <- z$L2/z$L1
      z$L3   <- z$TAU3*z$L2
      z$L4   <- z$TAU4*z$L2
      z$L5   <- z$TAU5*z$L2
    }
    else {
      z$LCV  <- vec[2]
      z$TAU3 <- vec[3]
      z$TAU4 <- vec[4]
      z$TAU5 <- vec[5]

      z$L2   <- z$LCV*z$L1
      z$L3   <- z$TAU3*z$L2
      z$L4   <- z$TAU4*z$L2
      z$L5   <- z$TAU5*z$L2
    }
    if(! are.lmom.valid(z)) {
      warning("L-moments are invalid, but still returning the values")
    }
    return(z) 
}
