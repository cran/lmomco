"partexp" <-
function(lmom, checklmom=TRUE) {
    para <- vector(mode="numeric", length=2)
    names(para) <- c("xi","alpha")
    if(checklmom & ! are.lmom.valid(lmom)) {
      warning("L-moments are invalid")
      return()
    }

    if(length(lmom$L1) == 0) { # convert to named L-moments
      lmom <- lmorph(lmom)     # nondestructive conversion!
    }
    T2 <- 0
    fn <- function(eta) {
       t2  <- (1 + 2*eta*log(eta) - eta^2) /
              ( 2 * (1 - eta) * (1 - eta + eta*log(eta) ) )
       #message("eta=",eta,"  t2=",t2,"  T2=",T2,"\n")
       return(abs(t2 - T2))
    }
    xmax <- 1E100
    T2min <- fn(xmax)

    L1 <- lmom$L1
    T2 <- lmom$LCV
    if(T2 < T2min) {
       warning("LCV ",T2," is lower than LCVmin=",T2min)
       return(NULL)
    }
    if(T2 > 1/2)   {
       warning("LCV ",T2," is greater than 1/2")
       return(NULL)
    }

    eps <- 1e-6

    if(T2 > 1/3 - .0001 & T2 < 1/3 + .0001) {
       return(list(type = 'texp', para = c(0, 2*L1), is.uni=TRUE,
                   source="partexp"))
    }

    ifelse(T2 > 1/3, inter <- c(0+eps, 1-eps),
                     inter <- c(1+eps, xmax))

    rot <- NA
    try(rot <- optimize(fn, inter), silent=FALSE)
    ETA <- rot$minimum

    para[2] <- (1-ETA) * L1 / (1 - ETA + ETA*log(ETA) )
    para[1] <- - para[2] * log(ETA)
    return(list(type = 'texp', para = para, is.uni=FALSE,
                source="partexp"))
}


