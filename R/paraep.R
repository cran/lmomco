"paraep" <-
function(lmom, checklmom=TRUE,
         method=c("Asquith", "Delicado-Goria", "Asquith_bypass_Delicado-Goria"),
         sqrt.t3t4=TRUE, eps=1e-3,
         A.guess=NULL, K.guess=NULL, H.guess=NULL) {

    method <- match.arg(method)

    if(checklmom && ! are.lmom.valid(lmom)) {
      warning("L-moments are invalid")
      return()
    }

    if(length(lmom$L1) != 0) { # convert to named L-moments
      lmom <- lmorph(lmom)     # nondestructive conversion!
    }

    para <- vector(mode="numeric", length=4)
    names(para) <- c("xi","alpha","kappa","h")
    para <- c(NA, NA, NA, NA);

    z <- list(type = 'aep', para = para, source="paraep",
              eachopt=list(para_L234 = para,
              ifail_L234=NA,
              optim.converg_L234=NA,
              optim.message_L234=NA,
              optim.value_L234=NA,
              optim.counts_L234=NA,
              para_T34 = para,
              ifail_T34=NA,
              optim.converg_T34=NA,
              optim.message_T34=NA,
              optim.value_T34=NA,
              optim.counts_T34=NA))

    L1 <- lmom$lambdas[1]
    L2 <- lmom$lambdas[2]
    L3 <- lmom$lambdas[3]
    L4 <- lmom$lambdas[4]
    T2 <- lmom$ratios[2]
    T3 <- lmom$ratios[3]
    T4 <- lmom$ratios[4]

    if(is.null(A.guess)) A.guess <- 1

    if(is.null(K.guess)) {
      if(abs(T3) < 0.1) {
         K.guess <- 1
      } else if(T3 > 0) { # right skew
         K.guess <- 0.5
      } else {
         K.guess <- 2
      }
    }
    if(is.null(H.guess)) {
      if(T4 < 0.1226017) { # normal
         H.guess <- 3
      } else {
         H.guess <- 0.33
      }
    }

    para.guess <- vec2par(c(0,A.guess,K.guess,H.guess), type="aep")
    if(! are.paraep.valid(para.guess)) {
       warning("One or more of the guess of A, K, and H (regardless of method choice) are invalid")
       return(z)
    }

    if(method == "Asquith_bypass_Delicado-Goria") {
      # do nothing
    } else {
      opt <- NULL
        "fn" <- function(ps, ...) {
             para <- list(para=c(0,ps), type="aep")
             slmr <- lmomaep(para, paracheck=FALSE)
             return(log(1 + (L2 - slmr$lambdas[2])^2
                          + (L3 - slmr$lambdas[3])^2
                          + (L4 - slmr$lambdas[4])^2))
        }
        try( opt <- optim(c(A.guess,K.guess,H.guess), fn), silent=TRUE)
          if(length(opt$par) == 0) return(z)
          para[2:4] <- opt$par
          A <- para[2]
          K <- para[3]
          H <- para[4]
          KmK <- 1/K - K
          H2H1 <- exp(lgamma(2/H) - lgamma(1/H))
          U <- L1 - A * KmK * H2H1
          para[1] <- U
          z$para = para
          z$eachopt$para_L234 = para
          z$eachopt$ifail_L234=as.logical(opt$convergence)
          z$eachopt$optim.converg_L234=opt$convergence
          z$eachopt$optim.message_L234=opt$message
          z$eachopt$optim.value_L234=opt$value
          z$eachopt$optim.counts_L234=opt$counts

         if(method == "Delicado-Goria") return(z)

         K.guess <- K
         H.guess <- H
      }

      "sqrtit" <- function(x) { return(x) }
      if(sqrt.t3t4) "sqrtit" <- function(x) { return(sqrt(x)) }

       opt <- NULL
      "fn" <- function(ps, ...) {
           para <- list(para=c(0,1,ps), type="aep")
           #print(para)
           slmr <- lmomaep(para, paracheck=FALSE, t3t4only=TRUE)
           return(sqrtit((T3 - slmr$T3)^2 + (T4 - slmr$T4)^2))
      }
      try( opt <- optim(c(K.guess,H.guess), fn), silent=TRUE)
         if(length(opt$par) == 0) return(z)
         para[3:4] <- opt$par
         K <- para[3]
         H <- para[4]
         KmK <- 1/K - K
         H2H1 <- exp(lgamma(2/H) - lgamma(1/H))
         Ihalf <- pbeta(1/2, shape1=1/H, shape2=2/H)
         KK    <- K*K
         KKK   <- KK*K

         L2a <- -K * KmK^2              / (1+KK)
         L2b <-  2 * KK * (1/KKK + KKK) / (1+KK)^2 * Ihalf
         A <- L2 / ((L2a + L2b) * H2H1)
         para[2] <- A
         U <- L1 - A * KmK * H2H1
         para[1] <- U
         z$para = para
         z$eachopt$para_T34 = para
         z$eachopt$ifail_T34=as.logical(opt$convergence)
         if(opt$value > eps) {
            warning("judging a solution failure based on eps value")
            z$eachopt$ifail_T34 <- TRUE
         }
         z$eachopt$optim.converg_T34=opt$convergence
         z$eachopt$optim.message_T34=opt$message
         z$eachopt$optim.value_T34=opt$value
         z$eachopt$optim.counts_T34=opt$counts
      #print(para)
      return(z)
}
