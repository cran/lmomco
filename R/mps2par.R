"mps2par" <-
function(x, type, para.int=NULL, ties=c("bernstein", "rounding", "density"),
            delta=0, log10offset=3, get.untied=FALSE, check.support=TRUE,
            silent=TRUE, moran=TRUE, ...) {
  x <- sort(x); n <- length(x); untied <- FALSE; ITS <- NA
  ties.method <- match.arg(ties)
  if(is.null(type)) {
     warning("must specify distribution type")
     return(NULL)
  }
  if(delta < 0) {
     warning("a delta argument < 0 does not make sense")
     return(NULL)
  }
  if(log10offset <= 0) {
     warning("a log10offset argument <= 0 does not make sense")
     return(NULL)
  }

  cfunc <- "1" # The following is experimental and following
  # a paper in the literature: Ghosh, K., and Jammalamadaka, S.R., 2001,
  # A general estimation method using spacings: Journal of Statistical Planning
  # and Inference, v. 93, pp. 71--82.
  the.cfunc <- switch(cfunc, "1" =function(x)     -log(x) ,
                             "2" =function(x) log(-log(x)),
                             "3" =function(x)    x*log(x) ,
                             "4" =function(x)          x^2,
                             "5" =function(x)    -sqrt(x) ,
                             "6" =function(x)        1/x  ,
                             "7" =function(x)    abs(x-1))
  # The Moran Test only if the cfunc() is "1".
  moran <- ifelse(moran & cfunc == "1", TRUE, FALSE)

  there_are_ties <- as.logical(sum(duplicated(x)))
  if(there_are_ties & ! silent) message("ties detected")
  if(there_are_ties & ! ties.method=="density") {
     untied <- TRUE; ITS <- 0; ix <- 1:n
     while( n != length(unique(x)) ) {
       ITS <- ITS + 1
       for(i in 1:n) { # loop the whole data sequence, avoid R idioms here!
         six <- ix[x == x[i]]; ntie <- length(six)
         if(ntie == 1) next # early out and move to next point
         if(ties.method == "bernstein") {
            half <- ntie/2 # initial estimate of the middle
            if(half != as.integer(half)) { # is odd, preserve one original value
              middle.idx    <- six[1] + as.integer(half)
              x.in.middle   <- x[middle.idx]
              x[six]        <- dat2bernqua(six/(n+1), x, ...)
              x[middle.idx] <- x.in.middle
              next;
            } else { # not able to preserve an original value because of symmetry
              x[six] <- dat2bernqua(six/(n+1), x, ...)
              next;
            }
            stop("shall not be here in logic!")
         }
         val <- x[six[1]]; tsub <- NA
         if(delta) {
            del <- delta
            ylo <- val - del; yhi <- val + del
            tsub <- seq(ylo, yhi, by=2*del/(ntie-1))
         } else {
            # This ifelse() is protection against zeros
            del <- ifelse(val == 0,                   -log10offset,
                          as.integer(log10(abs(val))) -log10offset)
            ylo <- val - 10^del; yhi <- val + 10^del
            tsub <- seq(ylo, yhi, by=2*10^del/(ntie-1))
         }
         if(length(tsub) != length(six)) {
            stop("fatal error in algorithmic thought")
         }
         x[six] <- tsub
       }
       if(ITS > 100) {
          warning("emergency break on trying to process ties, ",
                  "maximum iterations of 100 reached")
          break
       }
     }
  }
  x <- sort(x) # final insurance on the sort

  "afunc" <- function(para, x=NULL, n=NA, ...) {
     lmomco.para <- vec2par(para, type=type, paracheck=FALSE)
     #print(lmomco.para$para)
     if(is.null(lmomco.para)) return(Inf) # trap if bad parameters
     uu <- c(0,plmomco(x, lmomco.para),1) # padding the edges
     dd <- diff(uu) # the deltas, length shrinks by one
     if(ties == "density") {
        dd[dd == 0] <- dlmomco(x[dd == 0], lmomco.para)
     }
     M <- sum(the.cfunc(dd))
     if(! silent) message(" M=",M) # note M=0 will be shown but overrided
     if(M == 0) return(Inf) # this is key to keeping the simplex alive!
     return(M)
  }

  if(is.null(para.int)) {
     lmr <- lmoms(x)
     if(! are.lmom.valid(lmr)) {
        warning("L-moments of x are not valid, try manual initial parameters")
        return(NULL)
     }
     para.int <- lmom2par(lmr, type=type)
     if(is.null(para.int)) {
        warning("could not estimate initial parameters via L-moments")
        return(NULL)
     }
  }

  if(check.support) {
    # plotting position a must be [0,0.5], the 1 triggers max likelihood
    # and the 2 to trigger a last pass (testing the MLE) and then exit
    for(a.pp.coe in c(seq(0,0.5, by=0.1), 1, 2) ) {
      support <- supdist(para.int)$support
      if(min(x) < support[1]) {
         if(! silent) message("minimum x is < than the support of ",
                       "initial parameters, try alternative initial parameters")
         if(a.pp.coe <= 0.5) {
            if(! silent) message(" trying pwm.pp --> lmom --> a=",a.pp.coe)
            lmr <- pwm2lmom(pwm.pp(x, a=a.pp.coe))
            if(! are.lmom.valid(lmr)) next
            para.int <- lmom2par(lmr, type=type)
            next
         } else if(a.pp.coe == 1) {
            if(! silent) message(" trying MLE instead for initial parameters")
            para.int <- mle2par(x, type=type, ...)
            if(is.null(para.int)) return(NULL)
            next
         } else {
            warning(" giving up on automatic starting parameters")
            return(NULL)
         }
      }
      if(max(x) > support[2]) {
         if(! silent) message("maximum x is > than the support of ",
                       "initial parameters, try alternative initial parameters")
         if(a.pp.coe <= 0.5) {
            if(! silent) message(" trying pwm.pp --> lmom --> para for a=",a.pp.coe)
            lmr <- pwm2lmom(pwm.pp(x, a=a.pp.coe))
            if(! are.lmom.valid(lmr)) next
            para.int <- lmom2par(lmr, type=type)
            next
         } else if(a.pp.coe == 1) {
            if(! silent) message(" trying MLE instead for initial parameters")
            para.int <- mle2par(x, type=type, ...)
            if(is.null(para.int)) return(NULL)
            next
         } else {
            warning(" giving up on automatic starting parameters")
            return(NULL)
         }
      }
    }
  }
  if(is.null(para.int)) { # finally insurance policy for error trapping
     warning(" initial parameters are NULL")
     return(NULL)
  }
  rt <- NULL
  if(para.int$type != type) {
     warning("distribution requested to fit does not match the type of the ",
             "initial parameters")
     return(NULL)
  }
  try(rt <- optim(para.int$para, afunc, x=x, n=n, ...), silent=silent)
  if(is.null(rt)) {
     warning("optim() attempt is NULL")
     return(NULL)
  }
  lmomco.para          <- vec2par(rt$par, type=type)
  lmomco.para$source   <- "mps2par" # override vec2par()
  lmomco.para$para.int <- para.int  # preserve the initial parameters
  M <- rt$value
  if(! silent) lmomco.para$optim <- rt

  moranTest <- function(M,n,p) {
     moran_mean <- function(n) {
        g <- 0.57721566490153286060 # Euler's constant
        (n+1)*(log(n+1)+g) - (1/2) - 1/(12*(n+1))
     }
     moran_variance <- function(n, minusone=FALSE) {
        (n+1)*(pi^2/6 - 1) - (1/2) - 1/(6*(n+1))
     }
     muM <- moran_mean(n); varM <- moran_variance(n)
     C1    <- muM - sqrt(varM*n/2); C2 <- sqrt(varM/(2*n))
     Tstat <- (M - C1 + (p/2))/C2;  Io <- (n+1)*log(n+1)
     if(M < Io) {
       # The check.support should keep this from ever being triggered,
       # the author thinks!
       warning(" Moran < Io, ",
               "fitted distribution might have support that is ",
               "narrower than the data, failure results in Chi-Squared")
     }
     ests <- data.frame(muM=muM, varM=varM, C1=C1, C2=C2, sample.size=n)
     row.names(ests) <- "" # makes a print of output less confusing
     f <- pchisq(Tstat, n, lower.tail=TRUE);        p.value <- 1-f
           zz <- round(c(Io,   M,       Tstat,      p.value), digits=4)
     names(zz) <- c(    "Io", "Moran", "T(theta)", "p-value")
     return(list(diagnostics=ests, statistics=zz))
  }

  if(there_are_ties & get.untied & ! ties.method=="density") {
     lmomco.para$ties <- list(method=ties.method,
                              untied.pseudo.data=x, iterations=ITS)
  }

  if(moran) lmomco.para$MoranTest <- moranTest(M, n, length(lmomco.para$para))
  return(lmomco.para)
}

# Other implementations
# fBasics --> dist-gldFit.R --> .gldFit.mps -->
#          f = try(-typeFun(log(DH[DH > 0])), silent = TRUE)

# gld --> fit_fkml.R --> fit_fkml.c --> method.id == 2:
#If F[i]-F[i-1] = 0, replace by f[i-1] (ie the density at smaller observation)

# If the cfunc types are being explored:
#mean(replicate(1000, mps2par(rlmomco(100,list(type="nor", para=c(0.1,1))), type="nor", cfunc="3")$para[2]))
