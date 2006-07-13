"gen.freq.curves" <-
function(n,para,F=NULL,nsim=10,aslog=FALSE,asprob=FALSE,showparent=FALSE,...) {
  if(! are.par.valid(para)) return()
  type  <- para$type

  if(is.null(F) == TRUE) F <- nonexceeds()
  if(! check.fs(F)) return()
  plotF <- F
  if(asprob == TRUE) {
    plotF <- qnorm(F)
  }

  count <- 0
  while(count < nsim) {
    sX   <- rlmomco(n,para)
    sLMR <- lmoms(sX)
    if(! are.lmom.valid(sLMR)) {
      # yet another ad hoc solution for bizarre simulated values
      next
    }
    sPAR <- lmom2par(sLMR,type=type)
    if(is.null(sPAR) || ( length(sPAR$ifail) == 1
                                   && 
                            sPAR$ifail  != 0 ) ) {
      # The ifail is suitable for kappa (kap) and wakeby (wak)
      # distributions.
      # if the parameters could not be solved
      # for the desired distribution of the
      # parent---just do it again---ad hoc.
      next
    }
    count  <- count + 1

    freqcurve <- par2qua(F,sPAR)
    if(aslog == TRUE) freqcurve <- log10(freqcurve)
    lines(plotF,freqcurve,...)
  }
  if(showparent == TRUE) {
    parent <- par2qua(F,para)
    if(aslog == TRUE) parent <- log10(parent)
    lines(plotF,parent,lwd=3)
  }
}
