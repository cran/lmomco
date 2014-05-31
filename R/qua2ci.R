"qua2ci" <-
function(f,para,n, ci=0.90, edist="gno", nsim=1000,
         showpar=FALSE, empdist=TRUE, verbose=FALSE, maxlogdiff=6) {
  max.permitted.log10cycle.diff <- maxlogdiff
  ifail <- 0
  ifailtext <- "successful qua2ci() run"


  if(! check.fs(f)) {
     warning("The provided nonexceedance value was invalid")
     return()
  }
  if(length(f) > 1) {
     f <- f[1]
     warning("The nonexceedance probability was a vector, using only the first argument")
  }
  if(! are.par.valid(para)) {
     warning("The distribution parameters are invalid")
     return()
  }
  if(ci < 0.5 || ci >= 1) {
    warning("Confidence limit is specified by nonexceedance probability 0.5 <= ci < 1")
    return()
  }

  cilo <- (1-ci)/2
  cihi <- 1 - cilo

  Xtrue <- par2qua(f,para)
  type  <- para$type
  sQ    <- vector(mode = "numeric", length=nsim)
  count <- 0
  if(verbose == TRUE) {
    cat(c(nsim,"-"),sep="")
  }
  total.while <- 0
  while(count < nsim) {
    total.while <- total.while + 1
    sX   <- rlmomco(n,para)
    sLMR <- lmoms(sX)
    if(! are.lmom.valid(sLMR)) {
      # yet another ad hoc solution for bizarre simulated values
      next
    }
    sPAR <- lmom2par(sLMR, type=type)
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

    tmpQ  <- par2qua(f,sPAR) # save for the next test before loading into sQ
    if(log10(abs(tmpQ - Xtrue)) > max.permitted.log10cycle.diff ) {
      warning("qua2ci: Large difference between simulated quantile and the true seen by the maxlogdiff argument. repeating this simulation run.")
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
  ciPARlo <- ciPARhi <- NA
  if(! are.lmom.valid(ciLMR)) {
    ifail <- 1
    ifailtext <- "L-moments are invalid, poorly distributed simulated values, sample size too small for the complexity of the parent distribution"
    ciPAR <- NA
    ciPARlo <- NA
    ciPARhi <- NA
    if(verbose == TRUE) {
       cat(c("qua2ci:",ifailtext,"\n"),sep="")
    }
  }
  else {
    ciPAR <- lmom2par(ciLMR, type=edist)
    if(is.na(ciPAR$para[1])) {
      ifail <- 2
      ifailtext <- "L-moments are seemingly incompatable with the choosen error distribution"
      ciPAR <- NA
      ciPARlo <- NA
      ciPARhi <- NA
    } else {
      ciPARlo <- par2qua(cilo, ciPAR)
      ciPARhi <- par2qua(cihi, ciPAR)
    }
    if(verbose == TRUE) {
      pci <- 100*ci
      cat(c(pci,"-percent Confidence Interval\n"),sep="")
      cat("LowerCI      Prediction      UpperCI\n")
      cat(c(ciPARlo, Xtrue, ciPARhi,"\n"),sep="    ")
    }
  }
  emp.dist <- NA
  if(empdist) {
     emp.dist <- new.env()
     if(verbose == TRUE) {
        cat("Assigning the confidence limits by the empirical distribution using i/(n+1).\n")
     }
     assign("simquas", sQ, emp.dist)
     upper <- quantile(sQ, probs=cihi, type=6)
     lower <- quantile(sQ, probs=cilo, type=6)
     assign("empir.dist.lower", lower, emp.dist)
     assign("empir.dist.upper", upper, emp.dist)
     if(verbose == TRUE) {
        cat("Assigning the confidence limits by the Bernstein smoother.\n")
     }
     upper <- dat2bernqua(cihi, sQ, poly.type="Bernstein", bound.type="none")
     lower <- dat2bernqua(cilo, sQ, poly.type="Bernstein", bound.type="none")
     assign("bern.smooth.lower", lower, emp.dist)
     assign("bern.smooth.upper", upper, emp.dist)
     assign("epmoms", pmoms(sQ), emp.dist)
  }
  return(list(lower=ciPARlo, true=Xtrue, upper=ciPARhi,
              elmoms=ciLMR,  epara=ciPAR, empdist=emp.dist,
              ifail=ifail, ifailtext=ifailtext, nsim=nsim,
              sim.attempts=total.while))
}
