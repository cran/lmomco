"pargpa" <-
function(lmom) {
    para <- matrix(nrow = 3, ncol = 1)

    L1 <- NULL
    L2 <- NULL
    T3 <- NULL

    if(length(lmom$source) == 1 && lmom$source == "TLmoms") {
      if(lmom$trim != 0) {
        warning("Attribute of TL-moments is not trim=0--can not complete parameter estimation")
        return()
      }
      L1 <- lmom$lambdas[1]
      L2 <- lmom$lambdas[2]
      T3 <- lmom$ratios[3]
    }
    else if(length(lmom$L1) == 1) {
      L1 <- lmom$L1
      L2 <- lmom$L2
      T3 <- lmom$TAU3
    }
    else {
      warning("An L-moment object or TL-moment object was not provided as an argument")
      return()
    }

    if(! are.lmom.valid(lmom)) {
       warning("L-moments are invalid.")
       return()
    } 
    K <- (1-3*T3)/(1+T3)
    para[3] <- K
    para[2] <- (1+K)*(2+K)*L2
    para[1] <- L1 - para[2]/(1+K)
    return(list(type = 'gpa', para=para)) 
}

