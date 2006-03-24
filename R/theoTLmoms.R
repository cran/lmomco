"theoTLmoms" <- 
function(para,nmom=5,trim=NULL,leftrim=NULL,rightrim=NULL,verbose=FALSE) {
  if(nmom < 1) {
    warning("Number of TL-moments requested is less than 1")
    return()
  }
  if(! is.null(trim) && trim < 0) {
    warning("Trimming value is less than 0")
    return()
  }
  if(! is.null(leftrim) && leftrim < 0) {
    warning("Left rimming value is less than 0")
    return()
  }
  if(! is.null(rightrim) && rightrim < 0) {
    warning("Right trimming value is less than 0")
    return()
  }

  if(is.null(trim) && is.null(leftrim) && is.null(rightrim)) {
    trim <- 0
  }

  # t1 is the left-hand trimming and t2 is the right-hand trimming
  t1  <- NULL
  t2  <- NULL
  if(length(trim) == 1 && trim >= 0) {
    t1 <- trim
    t2 <- trim
    leftrim <- NULL
    rightrim <- NULL
  }
  else {
    trim <- NULL
    if(length(leftrim)  == 1 && leftrim  >= 0) t1 <- leftrim
    if(length(rightrim) == 1 && rightrim >= 0) t2 <- rightrim
    if(is.null(leftrim) ) { leftrim <- 0; t1 <- 0 }
    if(is.null(rightrim)) { rightrim <- 0; t2 <- 0 }
  }

  if(is.null(t1) || is.null(t2)) {
    warning("Ambiguous asymmetrical trimming values--use explicit leftrim and rightrim arguments")
    return()
  }

  L <- seq(1,nmom)
  R <- seq(1,nmom)
  for(r in seq(1,nmom)) { # for each  order of moment
    sum <- 0
    for(k in seq(0,r-1)) {
      tmp <- (-1)^k*choose(r-1,k)*factorial(r+t1+t2)/
                                 (factorial(r+t1-k-1)*
                                  factorial(t2+k))
      # Quantile function X(F), which will require numerical integration
      XofF <- function(F) {
        par2qua(F,para,paracheck=FALSE)*F^(r+t1-k-1)*(1-F)^(t2+k)
      }
      # Perform the numerical integration
      int <- integrate(XofF,0,1)
      # Sum up
      sum <- sum + tmp*int$value
      if(verbose == TRUE) { # Handy messages
        cat(c("abs.error=",int$abs.error,
              "  subdivisions=",int$subdivisions,
              "  message=",int$message,"\n"),sep="")
      }
    }
    L[r] <- sum/r  # don't forget to divide by the order of the L-moment
  }

  if(nmom >= 2) {
    R[2] <- L[2]/L[1]
  }
  if(nmom >= 3) {
    for(r in seq(3,nmom)) {
      R[r] <- L[r]/L[2]
    }
  }
  R[1] <- 0

  z <- list(lambdas = L, ratios = R, trim=trim,
            leftrim=leftrim, rightrim=rightrim, source="theoTLmoms")
  return(z)
}