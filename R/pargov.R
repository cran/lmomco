"pargov" <-
function(lmom,checklmom=TRUE) {
    para <- vector(mode="numeric", length=3)
    names(para) <- c("xi","alpha", "beta")
    if(length(lmom$L1) == 1) { # convert to named L-moments
      lmom <- lmorph(lmom)     # nondestructive conversion!
    }
    if(checklmom & ! are.lmom.valid(lmom)) {
      warning("L-moments are invalid")
      return()
    }
    L1 <- lmom$lambdas[1]
    L2 <- lmom$lambdas[2]
    T3 <- lmom$ratios[3]

    "t3f" <- function(b, t3) return(t3 - ((b - 2)/(b + 4)))
    tmp <- NULL
    try(tmp <- uniroot(t3f, lower=0, upper=100000, t3=T3), silent=TRUE)
    if(is.null(tmp)) {
      B <- NA
    } else {
      B <- tmp$root
    }
    A <- (B+2)*(B+3)*L2/(2*B)
    U <- L1 - 2*A/(B+2)
    para[1] <- U
    para[2] <- A
    para[3] <- B
    z <- list(para=para, type="gov", source="pargov")

    if(! are.pargov.valid(z)) {
        warning("After estimate, either A or B is <= 0")
    }
    return(z)
}

