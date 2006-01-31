"freq.curve.all" <-
function(lmom) {
    F <- nonexceeds()
    print("Exponential distribution")
    EXP <- freq.curve.exp(F,parexp(lmom))
    print("Gamma distribution")
    GAM <- freq.curve.gam(F,pargam(lmom))
    print("Generalized Extreme Value distribution")
    GEV <- freq.curve.gev(F,pargev(lmom))
    print("Generalized Logistic distribution")
    GLO <- freq.curve.glo(F,parglo(lmom))
    print("Generalized Normal distribution")
    GNO <- freq.curve.gno(F,pargno(lmom))
    print("Generalized Pareto distribution")
    GPA <- freq.curve.gpa(F,pargpa(lmom))
    print("Generalized Gumbel distribution")
    GUM <- freq.curve.gum(F,pargum(lmom))
    print("Kappa distribution")
    KAP <- freq.curve.kap(F,parkap(lmom))
    print("Normal distribution")
    NOR <- freq.curve.nor(F,parnor(lmom))
    print("Pearson Type III distribution")
    PE3 <- freq.curve.pe3(F,parpe3(lmom))
    print("Wakeby distribution")
    WAK <- freq.curve.wak(F,parwak(lmom))
    return(list(exp = EXP, gam = GAM, gev = GEV, glo = GLO,
                gno = GNO, gpa = GPA, gum = GUM, kap = KAP,
                nor = NOR, pe3 = PE3, wak = WAK))
}

