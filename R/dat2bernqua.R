"dat2bernqua" <-
function(f, x, natmin=NULL, natmax=NULL) {
    if(! check.fs(f)) return()
    x <- sort(x); n <- length(x)
    if(! is.null(natmin) && x[1] < natmin) {
       warning("The observed minimum is less than the declared natural minimum, resetting to observed minimum");
       natmin = NULL
    }
    if(! is.null(natmax) && x[n] > natmax) {
       warning("The observed maximum is greater than the declared natural maximum, resetting to observed maximum");
       natmax = NULL
    }
    qua <- vector(mode="numeric", length=length(f))
    for(i in 1:length(f)) {
       myf  <- f[i]
       mycf <- 1-myf
       tmp <- sapply(0:n, function(k) {
                      if(k == 0) {
                         xk <- ifelse(is.null(natmin), x[1], natmin)
                      } else {
                         xk <- x[k]
                      }
                      if(k == n) {
                         xkp1 <- ifelse(is.null(natmax), x[n], natmax)
                      } else {
                         xkp1 <- x[k+1]
                      }
                      return((xk+xkp1) * choose(n,k) * myf^k * mycf^(n-k))
                    })
       qua[i] <- sum(tmp)/2
    }
    return(qua)
}


