"parglo" <-
function(lmom) {
    SMALL <- 1e-6 
    # Estimate kappa of distribution
    para <- matrix(nrow = 3, ncol = 1)
    K <- -lmom$TAU3
    if(! are.lmom.valid(lmom)) {
       warning("L-moments are invalid.")
       return()
    } 
    if(abs(K) <= SMALL) {
      # kappa is effectively zero
      para[3] = 0
      para[2] = lmom$L2
      para[1] = lmom$L1
      return(list(type = 'glo', para = para))
    } 
    # Estimate alpha and xi of distribution
    KK <- K*pi/sin(K*pi) 
    A  <- lmom$L2/KK 
    para[1] <- lmom$L1 - A*(1-KK)/K
    para[2] <- A 
    para[3] <- K 
    return(list(type = 'glo', para = para)) 
}

