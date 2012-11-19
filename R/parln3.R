"parln3" <-
function(lmom, zeta=NULL, checklmom=TRUE) {

    if(length(lmom$L1) == 0) { # convert to named L-moments
      lmom <- lmorph(lmom)     # nondestructive conversion!
    }
    if(checklmom & ! are.lmom.valid(lmom)) {
       warning("L-moments are invalid")
       return()
    }

    L1 <- lmom$L1
    L2 <- lmom$L2
    T3 <- lmom$TAU3

    if(! is.null(zeta) && zeta >= (L1 - L2)) {
       warning("zeta is too large, must be zeta < Lamda1 - Lamda2")
       return()
    }

    if(is.null(zeta)) {
        if(is.na(T3)) stop("TAU3 is NA")
        gno   <- pargno(lmom)
        sigma <-  -gno$para[3]
        expmu <-   gno$para[2]/sigma
        para  <- c(gno$para[1] - expmu, log(expmu), sigma)
    }
    else {
       if(is.na(L2)) stop("L2 is NA")
       eta   <- L1 - zeta
       tmp   <- (1 + L2/eta)/2
       if(tmp < 0 | tmp > 1) {
          warning("bad zeta specified, inconsistent with L-moments")
       }
       sigma <- sqrt(2) * qnorm(tmp)
       mu    <- log(eta) - 0.5 * sigma^2
       para  <- c(zeta, mu, sigma)
    }

    names(para) <- c("zeta","mulog","sigmalog")

    return(list(type = "ln3",
                para=para, zeta=zeta, source="parln3"))
}
