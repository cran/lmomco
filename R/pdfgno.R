"pdfgno" <-
function(x,para) {
    # Error function from R documentation
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
    RTHALF <- 0.707106781186547524
    #  SMALL IS USED TO TEST WHETHER X IS EFFECTIVELY AT
    #  THE ENDPOINT OF THE DISTRIBUTION
    SMALL <- 1e-15

    if(! are.pargno.valid(para)) return()

    XI <- para$para[1]
    A  <- para$para[2]
    K  <- para$para[3]

    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      Y <- (x[i]-XI)/A
      if(K != 0) {
        ARG <- 1-K*Y
        if(ARG > SMALL) {
          Y <- -log(ARG)/K
        }
        else {
          if(K < 0) { f[i] <- 0; next }
          # K must be greater than zero--other end of distribution
          f[i] <- 1
          next
        }
      }
      f[i] <- exp(K*Y - Y^2/2)/(A*sqrt(2*pi))
    }
    return(f)
}

