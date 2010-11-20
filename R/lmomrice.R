"lmomrice" <-
function(para, ...) {
    V   <- para$para[1]
    A   <- para$para[2]
    if(V == 0) {
      ray <- vec2par(c(0,A), type="ray")
      lmr <- lmomray(para=ray)
      lmr$source <- "lmomrice"
      return(lmr)
    }
    lmr <- theoLmoms.max.ostat(para=para,
                cdf=cdfrice, pdf=pdfrice,
                lower=0,
                upper=.Machine$double.max, ...)
    lmr$source <- "lmomrice"
    if(! are.lmom.valid(lmr)) {
      warning("The Rician parameters are producing invalid L-moments or L-moments outside of implementation of Rice distribution in lmomco")
      print(para)
      print(lmr)
      return()
    }
    return(lmr)
}

