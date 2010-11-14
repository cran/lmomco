"lmomrice" <-
function(para) {
    lmr <- theoLmoms.max.ostat(para=para, cdf=cdfrice, pdf=pdfrice,
                               lower=0, upper=.Machine$double.max)
    lmr$source <- "lmomrice"
    if(! are.lmom.valid(lmr)) {
      warning("L-moments are invalid or outside of implementation of Rice distribution in lmomco")
      print(lmr)
      return()
    }
    return(lmr)
}

