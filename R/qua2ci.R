"qua2ci" <-
function(f,para,n,ci=0.90,edist='nor',nsim=1000,
         verbose=FALSE,showpar=FALSE,maxlogdiff=6) {
  maximum.permitted.log10cycle.difference <- maxlogdiff
  ifail <- 0
  ifailtext <- "successful qua2ci() run"
  if(! check.fs(f)) return()
  if(! are.par.valid(para)) return()
  if(ci < 0.5 || ci >= 1) {
    warning("Confidence limit is specified by nonexceedance probability 0.5 <= ci < 1")
    return()
  }
  Xtrue <- par2qua(f,para)
  type  <- para$type
  sQ    <- vector(mode = "numeric")
  count <- 0
  if(verbose == TRUE) {
    cat(c(nsim,"-"),sep="")
  }
  while(count < nsim) {
    sX   <- rlmomco(n,para)
    sLMR <- lmoms(sX)
    if(! are.lmom.valid(sLMR)) {
      # yet another ad hoc solution for bizarre simulated values
      next
    }
    sPAR <- lmom2par(sLMR,type=type)
    if(showpar == TRUE) print(sPAR)
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
    tmpQ      <- par2qua(f,sPAR) # save for the next test before loading into sQ
    if(log10(abs(tmpQ - Xtrue)) > maximum.permitted.log10cycle.difference ) {
      if(verbose == TRUE) {
        cat("qua2ci: Large difference between simulated quantile and the true seen. Skipping this simulation run.\n")
      }
      next
    }
    count     <- count + 1
    sQ[count] <- tmpQ
    if(verbose == TRUE) {
      cat(c(nsim-count,"-"),sep="")
    }
  }
  if(verbose == TRUE) {
    cat("\n")
  }
  ciLMR <- lmoms(sQ)
  ciPMR <- pmoms(sQ)
  if(! are.lmom.valid(ciLMR)) {
    ifail <- 1
    ifailtext <- "L-moments are invalid, poorly distributed simulated values, sample size too small for the complexity of the parent distribution"
    ciPAR <- NA
    upper <- NA
    lower <- NA
    if(verbose == TRUE) {
       cat(c("qua2ci:",ifailtext,"\n"),sep="")
    }
  }
  else {
    ciPAR <- lmom2par(ciLMR,type=edist)
    if(is.na(ciPAR$para[1])) {
      ifail <- 2
      ifailtext <- "L-moments are seemingly incompatable with choosen error distribution"
      ciPAR <- NA
      upper <- NA
      lower <- NA
    } else {
      upper <- par2qua(1-(1-ci)/2,ciPAR)
      lower <- par2qua((1-ci)/2,ciPAR)
    }
    if(verbose == TRUE) {
      pci <- 100*ci
      cat(c(pci,"-percent Confidence Interval\n"),sep="")
      cat("LowerCI      Prediction      UpperCI\n")
      cat(c(lower,Xtrue,upper,"\n"),sep="    ")
    }
  }
  return(list(lower=lower, true=Xtrue, upper=upper,
              elmoms=ciLMR, epmoms=ciPMR, epara=ciPAR,
              ifail=ifail, ifailtext=ifailtext))
}
