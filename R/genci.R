"genci" <-
function(para,n,F=NULL,ci=0.90,edist='nor',nsim=1000,expand=FALSE,
         verbose=FALSE,showpar=FALSE) {
  if(is.null(F) == TRUE) F <- nonexceeds()
  if(! check.fs(F)) return()
  if(! are.par.valid(para)) return()
  if(ci < 0.5 || ci >= 1) {
    warning("Confidence limit is specified by nonexceedance probability 0.5 <= ci < 1")
    return()
  }
  ci_low <- vector(mode = 'numeric')
  ci_tru <- vector(mode = 'numeric')
  ci_hi  <- vector(mode = 'numeric')
  ci_l1  <- vector(mode = 'numeric')
  ci_l2  <- vector(mode = 'numeric')
  ci_t3  <- vector(mode = 'numeric')
  ci_t4  <- vector(mode = 'numeric')
  ci_t5  <- vector(mode = 'numeric')
  n <- length(F)
  cat(c(n,"-"),sep="")
  for(i in seq(1,n)) {
    CI <- qua2ci(F[i],para,n,ci=ci,edist=edist,nsim=nsim,verbose=verbose,showpar=showpar)
    ci_low[i] <- CI$lower
    ci_tru[i] <- CI$true
    ci_hi[i]  <- CI$upper
    ci_l1[i]  <- CI$elmoms$lambdas[1]
    ci_l2[i]  <- CI$elmoms$lambdas[2]
    ci_t3[i]  <- CI$elmoms$ratios[3]
    ci_t4[i]  <- CI$elmoms$ratios[4]
    ci_t5[i]  <- CI$elmoms$ratios[5]
    cat(c(n-i,"-"),sep="")
  }
  cat("\n")
  cis <- data.frame(nonexceed_prob=F,lower=ci_low,true=ci_tru,upper=ci_hi,lscale=ci_l2)
  lmr <- data.frame(lambda1=ci_l1, lambda2=ci_l2, 
                    tau3=ci_t3, tau4=ci_t4, tau5=ci_t5)
  if(expand == TRUE) {
    return(list(limits=cis,parent=para,edist=edist,elmoms=lmr))
  }
  else {
    return(cis)
  }
}
