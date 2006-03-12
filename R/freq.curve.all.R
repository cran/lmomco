"freq.curve.all" <-
function(lmom,aslog10=FALSE,asprob=TRUE,
              no2para=FALSE,no3para=FALSE,no4para=FALSE,no5para=FALSE,
              step=FALSE,show=FALSE,
              xmin=NULL,xmax=NULL,xlim=NULL,
              ymin=NULL,ymax=NULL,ylim=NULL,
              exp=TRUE,gam=TRUE,gev=TRUE,gld=FALSE,glo=TRUE,
              gno=TRUE,gpa=TRUE,gum=TRUE,kap=TRUE,nor=TRUE,pe3=TRUE,wak=TRUE,...) {

    if(no2para) {
     exp <- FALSE
     gam <- FALSE
     gum <- FALSE
     nor <- FALSE
    }
    if(no3para) {
     gev <- FALSE
     glo <- FALSE
     gno <- FALSE
     gpa <- FALSE
     pe3 <- FALSE
    }
    if(no4para) {
     gld <- FALSE
     kap <- FALSE
    }
    if(no5para) {
     wak <- FALSE
    }

    F <- nonexceeds()
    n <- length(F)
    EXP <- vector(mode="numeric",length=n)
    GAM <- vector(mode="numeric",length=n)
    GEV <- vector(mode="numeric",length=n)
    GLD <- vector(mode="numeric",length=n)
    GLO <- vector(mode="numeric",length=n)
    GNO <- vector(mode="numeric",length=n)
    GPA <- vector(mode="numeric",length=n)
    GUM <- vector(mode="numeric",length=n)
    KAP <- vector(mode="numeric",length=n)
    NOR <- vector(mode="numeric",length=n)
    PE3 <- vector(mode="numeric",length=n)
    WAK <- vector(mode="numeric",length=n)

    if(exp == TRUE) {
      if(show == TRUE) cat("Exponential distribution--")
      P <- parexp(lmom)
      if(show == TRUE) cat("parameters--")
      EXP <- freq.curve.exp(F,P)
      if(aslog10 == TRUE) EXP <- log10(EXP)
      if(show == TRUE) cat("quantiles\n")
    }
    if(gam == TRUE) {
      if(show == TRUE) cat("Gamma distribution--")
      P <- pargam(lmom)
      if(show == TRUE) cat("parameters--")
      GAM <- freq.curve.gam(F,P)
      if(aslog10 == TRUE) GAM <- log10(GAM)
      if(show == TRUE) cat("quantiles\n")
    }
    if(gev == TRUE) {
      if(show == TRUE) cat("Generalized Extreme Value distribution--")
      P <- pargev(lmom)
      if(show == TRUE) cat("parameters--")
      GEV <- freq.curve.gev(F,P)
      if(aslog10 == TRUE) GEV <- log10(GEV)
      if(show == TRUE) cat("quantiles\n")
    }
    if(gld == TRUE) {
      if(show == TRUE) cat("Generalized Lambda distribution (takes awhile)--")
      P <- pargld(lmom,...)
      if(show == TRUE) cat("parameters--")
      GLD <- freq.curve.gld(F,P)
      if(aslog10 == TRUE) GLD <- log10(GLD)
      if(show == TRUE) cat("quantiles\n")
    }
    if(glo == TRUE) {
      if(show == TRUE) cat("Generalized Logistic distribution--")
      P <- parglo(lmom)
      if(show == TRUE) cat("parameters--")
      GLO <- freq.curve.glo(F,P)
      if(aslog10 == TRUE) GLO <- log10(GLO)
      if(show == TRUE) cat("quantiles\n")
    }
    if(gno == TRUE) {
      if(show == TRUE) cat("Generalized Normal distribution--")
      P <- pargno(lmom)
      if(show == TRUE) cat("parameters--")
      GNO <- freq.curve.gno(F,P)
      if(aslog10 == TRUE) GNO <- log10(GNO)
      if(show == TRUE) cat("quantiles\n")
    }
    if(gpa == TRUE) {
      if(show == TRUE) cat("Generalized Pareto distribution--")
      P <- pargpa(lmom)
      if(show == TRUE) cat("parameters--")
      GPA <- freq.curve.gpa(F,P)
      if(aslog10 == TRUE) GPA <- log10(GPA)
      if(show == TRUE) cat("quantiles\n")
    }
    if(gum == TRUE) {
      if(show == TRUE) cat("Generalized Gumbel distribution--")
      P <- pargum(lmom)
      if(show == TRUE) cat("parameters--")
      GUM <- freq.curve.gum(F,P)
      if(aslog10 == TRUE) GUM <- log10(GUM)
      if(show == TRUE) cat("quantiles\n")
    }
    if(kap == TRUE) {
      if(show == TRUE) cat("Kappa distribution--")
      P <- parkap(lmom)
      if(show == TRUE) cat("parameters--")
      KAP <- freq.curve.kap(F,P)
      if(aslog10 == TRUE) KAP <- log10(KAP)
      if(show == TRUE) cat("quantiles\n")
    }
    if(nor == TRUE) {
      if(show == TRUE) cat("Normal distribution--")
      P <- parnor(lmom)
      if(show == TRUE) cat("parameters--")
      NOR <- freq.curve.nor(F,P)
      if(aslog10 == TRUE) NOR <- log10(NOR)
      if(show == TRUE) cat("quantiles\n")
    }
    if(pe3 == TRUE) {
      if(show == TRUE) cat("Pearson Type III distribution--")
      P <- parpe3(lmom)
      if(show == TRUE) cat("parameters--")
      PE3 <- freq.curve.pe3(F,P)
      if(aslog10 == TRUE) PE3 <- log10(PE3)
      if(show == TRUE) cat("quantiles\n")
    }
    if(wak == TRUE) {
      if(show == TRUE) cat("Wakeby distribution--")
      P <- parwak(lmom)
      if(show == TRUE) cat("parameters--")
      WAK <- freq.curve.wak(F,P)
      if(aslog10 == TRUE) WAK <- log10(WAK)
      if(show == TRUE) cat("quantiles\n")
    }
    
    Q <- data.frame(nonexceeds = F,exp = EXP, gam = GAM, gev = GEV, glo = GLO,
                                   gld = GLD, gno = GNO, gpa = GPA, gum = GUM,
		                   kap = KAP, nor = NOR, pe3 = PE3, wak = WAK)
    if(show == TRUE) {
      xlab <- "NONEXCEEDANCE PROBABILITY"
      if(asprob == TRUE) {
        F <- qnorm(F)
        xlab <- "STANDARD NORMAL DEVIATE"     
      }

      limx <- range(F,finite=TRUE)
      if(length(xmin) == 1) limx[1] <- xmin
      if(length(xmax) == 1) limx[2] <- xmin
      if(length(xlim) == 2) limx    <- xlim

      limy <- range(EXP,GAM,GEV,GLO,GLD,GNO,GPA,GUM,KAP,NOR,PE3,WAK,finite=TRUE)
      if(length(ymin) == 1) limy[1] <- ymin
      if(length(ymax) == 1) limy[2] <- ymin
      if(length(ylim) == 2) limy    <- ylim
      plot(F,F,type='n',xlim=c(limx[1],limx[2]),
                        ylim=c(limy[1],limy[2]),ylab="QUANTILES",xlab=xlab)
      lines(F,Q$exp,col=2)
      lines(F,Q$gam,col=2)
      lines(F,Q$gev,col=3)
      lines(F,Q$glo,col=3)
      lines(F,Q$gld,col=4,lty=2,lwd=2)
      lines(F,Q$gno,col=3)
      lines(F,Q$gpa,col=3)
      lines(F,Q$gum,col=2)
      lines(F,Q$kap,col=4,lwd=2)
      lines(F,Q$nor,col=1)
      lines(F,Q$pe3,col=3)
      lines(F,Q$wak,col=6)
    }

    return(Q)
}

