"vec2TLmom" <-
function(vec,trim=NULL,lscale=TRUE) {
    if(is.null(trim)) {
      warning("Argument trim is not defined")
      stop()
    }
    
    L <- seq(1,5)
    R <- seq(1,5)
    L[1] <- vec[1]
    R[1] <- 0

    if(lscale == TRUE) {
      L[2] <- vec[2]
      R[3] <- vec[3]
      R[4] <- vec[4]
      R[5] <- vec[5]

      R[2] <- L[2]/L[1]
      L[3] <- R[3]*L[2]
      L[4] <- R[4]*L[2]
      L[5] <- R[5]*L[2]
    }
    else {
      R[2] <- vec[2]
      R[3] <- vec[3]
      R[4] <- vec[4]
      R[5] <- vec[5]

      L[2] <- R[2]*L[1]
      L[3] <- R[3]*L[2]
      L[4] <- R[4]*L[2]
      L[5] <- R[5]*L[2]
    }
    z <- list(lambdas = L, ratios = R, trim=trim, source="TLmoms")
    return(z) 
}
