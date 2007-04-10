"gen.freq.curves" <-
function(n,para,F=NULL,nsim=10,callplot=TRUE,aslog=FALSE,asprob=FALSE,
           showsample=FALSE,showparent=FALSE,...) {
  if(! are.par.valid(para)) return()
  type  <- para$type

  if(is.null(F) == TRUE) F <- nonexceeds()
  if(! check.fs(F)) return()
  plotF  <- F
  xlabel <- 'NONEXCEEDANCE PROBABILITY'
  ylabel <- 'QUANTILE'
  if(asprob == TRUE) {
    plotF  <- qnorm(F)
    xlabel <- 'STANDARD NORMAL QUANTILE'
  }

  if(callplot == TRUE) {
    Q <- par2qua(F,para)
    if(aslog == TRUE) {
      Q <- log10(Q)
      ylabel <- 'log10(QUANTILE)'
    }
    plot(plotF,Q,type='n',xlab=xlabel,ylab=ylabel)
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
    if(showsample == TRUE) {
      sX <- sort(sX)
      plotting.position <- pp(sX)
      if(asprob == TRUE) plotting.position <- qnorm(plotting.position)
      if(aslog == TRUE)  sX <- log10(sX)
      points(plotting.position,sX,...)
    }
  }
  if(showparent == TRUE) {
    parent <- par2qua(F,para)
    if(aslog == TRUE) parent <- log10(parent)
    lines(plotF,parent,lwd=3)
  }
}
