"lmomkmu" <-
function(para, nmom=5, paracheck=TRUE, tol=1E-6, maxn=100) {

    if(paracheck && ! are.parkmu.valid(para)) {
       warning("Parameters are invalid")
       return()
    }

    names(para$para) <- NULL
    K <- para$para[1]
    M <- para$para[2]

    "marcumq.integral" <- function(a, b, nu=NULL) {
       if(is.null(nu)) {
          warning("nu is NULL for Marcum Q function, returning NA")
          return(NA)
       }
       if(a == 0) a <- .Machine$double.eps
       "afunc" <- function(t) {
           B <- vector(mode="numeric", length=length(t))
           B <- sapply(1:length(B), function(i) {
                  toI <- a * t[i]
                  b <- besselI(toI, nu=nu-1)
                  if(! is.finite(b)) b <- .Machine$double.xmax
                  return(b) })
           z <- t^nu * exp(-(t^2 + a^2)/2) * B
           return( z )
       }
       int1 <- NULL
       try( int1 <- integrate(afunc, lower=b, upper=Inf) )
       if(is.null(int1)) return(NA)
       return( int1$value/a^(nu-1) )
    }

    # Shi, Q., Karasawa, Y., 2012, An intuitive methodology
    # for efficient evaluation of the Nuttall Q-function and
    # performance analysis of energy detection in fading
    # channels: IEEE Wireless Communications Letters,
    # v. 1, no. 2, pp. 109--112.
    "marcumq.bydelta" <- function(a, b, nu=NULL) {
       if(is.null(nu)) {
          warning("nu is NULL for Marcum Q function, returning NA")
          return(NA)
       }
       delta <- nu %% as.integer(nu); nuint <- as.integer(nu)
       if(is.nan(delta)) { delta <- nu; nuint <- 0 }
       if(nu >  0) { beg <- 0;     end <- nuint - 1; sign <-  1 }
       if(nu <= 0) { beg <- nuint; end <-       - 1; sign <- -1 }
       if(nuint == 0) { sign <- 0 }
       if(a == 0) a <- .Machine$double.eps
       Qdelta <- marcumq.integral(a, b, nu=delta)
       tmp <- ifelse(sign == 0, 0,
                sum(sapply(beg:end, function(i) { (b/a)^(i+delta) *
                        besselI(a*b, nu=i+delta) })))
       Qnuint <- sign * exp(-(a^2+b^2)/2) * tmp
       if(is.nan(Qnuint)) Qnuint <- 0
       Qnu <- Qdelta + Qnuint
       #message("nuint=", nuint, "  delta=",delta,"  beg=",beg,"  end=",end,"   sign=",sign)
       #message("Qdelta=",Qdelta,"  Qnuint=",Qnuint,"\n")
       if(is.nan(Qnu)) Qnu <- 0
       return(Qnu)
    }

    marcumq <- marcumq.bydelta

    A <- sqrt(2*K*M)
    B <- sqrt(2*(1+K)*M)
    afunc <- function(x, r=0) {
       Q <- sapply(1:length(x), function(i) { return(marcumq(A,B*x[i], nu=M)) } )
       pdf <- pdfkmu(x, para=para, paracheck=FALSE)
       W <- Q^r * x * pdf
       return(W)
    }

    alphas <- rep(NA, length(nmom))
    for(r in 1:nmom) {
         int <- NULL
         try(int <- integrate(afunc, 0, Inf, subdivisions=100, r=(r-1)), silent=TRUE)
         if(is.null(int)) {
            alphas[r] <- NaN;
            next;
         }
         alpha <- int$value
         alphas[r] <- alpha
    }
    z <- pwm2lmom(pwm.alpha2beta(alphas))
    z$source <- "lmomkmu"
    return(z)
}


