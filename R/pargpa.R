"pargpa" <-
function(lmom) {
    para <- matrix(nrow = 3, ncol = 1)
    L1 <- lmom$L1
    L2 <- lmom$L2
    T3 <- lmom$TAU3
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

