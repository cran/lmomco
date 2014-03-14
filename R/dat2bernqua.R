"dat2bernqua" <-
function(f, x, bern.control=NULL,
               poly.type=c("Bernstein", "Kantorovich", "Cheng"),
               bound.type=c("none", "sd", "Carv", "either"),
               fix.lower=NULL, fix.upper=NULL, p=0.05,
               listem=FALSE) {

    if(! is.null(bern.control)) {
         poly.type  <- bern.control$poly.type
         bound.type <- bern.control$bound.type
         fix.lower  <- bern.control$fix.lower
         fix.upper  <- bern.control$fix.upper
         p          <- bern.control$p
    }

    if(p < 1E-6 || p >= (1 - 1E-6)) {
        warning("p is too small or too large (ad hoc decision, see source code), returning NA")
        return(NA)
    }
    poly.type <- match.arg(poly.type)
    bound.type <- match.arg(bound.type)
    if(! check.fs(f)) return()
    x <- sort(x); n <- length(x)
    if(! is.null(fix.lower) && x[1] < fix.lower) {
       warning("The observed minimum is less than the declared lower bounds, resetting to observed minimum")
       fix.lower <- x[1]
    }
    if(! is.null(fix.upper) && x[n] > fix.upper) {
       warning("The observed maximum is greater than the declared upper bounds, resetting to observed maximum")
       fix.upper <- x[n]
    }
    lam2 <- lmoms(x, nmom=2)$lambdas[2]
    # Compute sd-based bounds
    sd.lower <- x[1] - lam2*sqrt(pi/n)
    sd.upper <- x[n] + lam2*sqrt(pi/n)
    # Compute de Carvalho bounds
    a <- (1-p)^(-2) - 1
    Carv.lower <- x[1] - (x[2] - x[1])/a
    Carv.upper <- x[n] + (x[n] - x[(n-1)])/a
    if(bound.type == "sd") {
        #message("Using the standard deviation support if either end is larger (smaller) than the respective data minimum (maximum)")
        fix.lower <- max(c(sd.lower, fix.lower))
        fix.upper <- min(c(sd.upper, fix.upper))
    } else if(bound.type == "Carv") {
        #message("Using the de Carvalho support if either end is larger (smaller) than the respective data minimum (maximum)")
        fix.lower <- max(c(Carv.lower, fix.lower))
        fix.upper <- min(c(Carv.upper, fix.upper))
    } else if(bound.type == "either") {
        #message("Using either the standard deviation or de Carvalho support if either end is larger (smaller) than the respective data minimum (maximum)")
        fix.lower <- max(c(sd.lower, Carv.lower, fix.lower))
        fix.upper <- min(c(sd.upper, Carv.upper, fix.upper))
    } else if(bound.type == "none") {
        if(is.null(fix.lower)) fix.lower <- x[1]
        if(is.null(fix.upper)) fix.upper <- x[n]
    } else {
       # Do nothing
    }
    qua <- vector(mode="numeric", length=length(f))
    for(i in 1:length(f)) {
       myf  <- f[i]
       if(poly.type == "Cheng") {
             tmp <- sapply(1:n, function(k) {
                        xk <- x[k]
                        return(xk * choose(n-1,k-1) * myf^(k-1) * (1-myf)^(n-k)) })
             qua[i] <- sum(tmp)
       } else {
          if(poly.type == "Bernstein") {
             tmp <- sapply(0:(n+1), function(k) {
                        xk <- x[k]
                        if(k ==     0) xk <- fix.lower
                        if(k == (n+1)) xk <- fix.upper
                        return(xk * choose(n+1,k) * myf^k * (1-myf)^(n+1-k)) })
             qua[i] <- sum(tmp)
          } else if(poly.type == "Kantorovich") {
             tmp <- sapply(0:n, function(k) {
                        xk   <- ifelse(k == 0, fix.lower, x[k]  )
                        xkp1 <- ifelse(k == n, fix.upper, x[k+1])
                        return((xk+xkp1) * choose(n,k) * myf^k * (1-myf)^(n-k)) })
             qua[i] <- sum(tmp)/2
          } else {
             stop("Should not be here in the logic")
          }
       }
    }
    if(listem) {
       z <- list(f=f, x=qua, n=n, p=p,
                 fix.lower=fix.lower, fix.upper=fix.upper)
       return(z)
    } else {
       return(qua)
    }
}


