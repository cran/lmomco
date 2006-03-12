"parnor" <-
function(lmom) {
    para <- vector(mode="numeric", length=2)
    if(! are.lmom.valid(lmom)) {
      warning("L-moments are invalid.")
      return()
    }
    para[1] <- lmom$L1
    para[2] <- lmom$L2*sqrt(pi)
    return(list(type = 'nor', para=para))
}

