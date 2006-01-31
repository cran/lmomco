"parexp" <-
function(lmom) {
    para <- matrix(nrow = 2, ncol = 1)
    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      return()
    }
    para[2] <- 2*lmom$L2
    para[1] <- lmom$L1 - para[2]
    return(list(type = 'exp', para = para))
}

