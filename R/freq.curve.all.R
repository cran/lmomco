"freq.curve.all" <-
function(lmom,exp=TRUE,gam=TRUE,gev=TRUE,gld=TRUE,glo=TRUE,
              gno=TRUE,gpa=TRUE,gum=TRUE,kap=TRUE,nor=TRUE,pe3=TRUE,wak=TRUE,...) {
    F <- nonexceeds()
    EXP <- NULL
    GAM <- NULL
    GEV <- NULL
    GLD <- NULL
    GLO <- NULL
    GNO <- NULL
    GPA <- NULL
    GUM <- NULL
    KAP <- NULL
    NOR <- NULL
    PE3 <- NULL
    WAK <- NULL

    if(exp == TRUE) {
      cat("Exponential distribution\n")
      EXP <- freq.curve.exp(F,parexp(lmom))
    }
    if(gam == TRUE) {
      cat("Gamma distribution\n")
      GAM <- freq.curve.gam(F,pargam(lmom))
    }
    if(gev == TRUE) {
      cat("Generalized Extreme Value distribution\n")
      GEV <- freq.curve.gev(F,pargev(lmom))
    }
    if(gld == TRUE) {
      cat("Generalized Lambda distribution\n")
      GLD <- freq.curve.gld(F,pargld(lmom,...))
    }
    if(glo == TRUE) {
      cat("Generalized Logistic distribution\n")
      GLO <- freq.curve.glo(F,parglo(lmom))
    }
    if(gno == TRUE) {
      cat("Generalized Normal distribution\n")
      GNO <- freq.curve.gno(F,pargno(lmom))
    }
    if(gpa == TRUE) {
      cat("Generalized Pareto distribution\n")
      GPA <- freq.curve.gpa(F,pargpa(lmom))
    }
    if(gum == TRUE) {
      cat("Generalized Gumbel distribution\n")
      GUM <- freq.curve.gum(F,pargum(lmom))
    }
    if(kap == TRUE) {
      cat("Kappa distribution\n")
      KAP <- freq.curve.kap(F,parkap(lmom))
    }
    if(nor == TRUE) {
      cat("Normal distribution\n")
      NOR <- freq.curve.nor(F,parnor(lmom))
    }
    if(pe3 == TRUE) {
      cat("Pearson Type III distribution\n")
      PE3 <- freq.curve.pe3(F,parpe3(lmom))
    }
    if(wak == TRUE) {
      cat("Wakeby distribution\n")
      WAK <- freq.curve.wak(F,parwak(lmom))
    }

    return(list(exp = EXP, gam = GAM, gev = GEV, glo = GLO,
                gld = GLD, gno = GNO, gpa = GPA, gum = GUM,
		kap = KAP, nor = NOR, pe3 = PE3, wak = WAK))
}

