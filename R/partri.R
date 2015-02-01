"partri" <-
function(lmom,checklmom=TRUE) {

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
    Xg <- L1 + L2
    Ng <- L1 - L2
    if(abs(T3) > 0.1428572) {
       warning("L-skew is likely incompatible with the distribution")
       # It has been hard to evaluate whether this hack here on high skew
       # is that useful but code seems to work.
       #if(sign(T3) == -1) {
       #   Og <- Xg
       #} else {
       #   Og <- Ng
       #}
    }# else {
       Og <- (Xg + Ng)/2
    #}

    "afunc" <- function(par) {
       z <- list(para=par, type="tri")
       lmr <- lmomtri(z, paracheck=FALSE)
       err <- ((lmr$lambdas[1] - L1)/L1)^2 +
              ((lmr$lambdas[2] - L2)/L2)^2 +
              ((lmr$ratios[3]  - T3)   )^2
       return(err)
    }

    opt <- NULL
    try(opt <- optim(c(Ng, Og, Xg), afunc))
    if(is.null(opt)) {
       warning("could not simultaneously optimize the three parameters, returning NULL")
       return(NULL)
    }
    if(opt$convergence != 0) {
       warning("convergence problems, printing out results")
       print(opt)
    }
    if(opt$par[2] > opt$par[3] | opt$par[2] < opt$par[1]) {
       message("likely just numerical flux if the distribution is a or nearly a ",
               "right triangle but the mode is incompatible with one or the limits, ",
               "proceeding to sort the parameter estimates")
       message("Original: ",paste(opt$par, collapse=" "))
       para <- sort(opt$par)
    } else {
       para <- opt$par
    }
    if(T3 > 0.142857) { # snap to a right triangle
       min <- (para[1]+para[2])/2
       para[1] <- para[2] <- min
    }
    if(T3 < -0.142857) { # snap to a right triangle
       max <- (para[2]+para[3])/2
       para[2] <- para[3] <- max
    }
    names(para) <- c("min","mode","max")

    return(list(type = 'tri', para = para, obj.val=opt$value, source="partri"))
}

