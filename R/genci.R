"genci" <-
function(para,n,F=NULL,ci=0.90,edist='nor',
         nsim=1000,expand=FALSE,
         verbose=FALSE,showpar=FALSE,quiet=FALSE) {
  if(is.null(F)) F <- nonexceeds()
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
  ci_mu  <- vector(mode = 'numeric')
  ci_var <- vector(mode = 'numeric')
  ci_skw <- vector(mode = 'numeric')
  num.Fs <- length(F)
  if(! quiet) cat(c(num.Fs,"-"),sep="")
  for(i in seq(1,num.Fs)) {
    CI <- qua2ci(F[i], para, n,
                 ci=ci, edist=edist, nsim=nsim,
                 verbose=verbose,
                 showpar=showpar)
    if(CI$ifail > 0) {
       ci_low[i] <- NA
       ci_tru[i] <- NA
       ci_hi[i]  <- NA
       ci_l1[i]  <- NA
       ci_l2[i]  <- NA
       ci_t3[i]  <- NA
       ci_t4[i]  <- NA
       ci_t5[i]  <- NA
       ci_mu[i]  <- NA
       ci_var[i] <- NA
       ci_skw[i] <- NA
       next
    }
    ci_low[i] <- CI$lower
    ci_tru[i] <- CI$true
    ci_hi[i]  <- CI$upper
    ci_l1[i]  <- CI$elmoms$lambdas[1]
    ci_l2[i]  <- CI$elmoms$lambdas[2]
    ci_t3[i]  <- CI$elmoms$ratios[3]
    ci_t4[i]  <- CI$elmoms$ratios[4]
    ci_t5[i]  <- CI$elmoms$ratios[5]
    ci_mu[i]  <- CI$epmoms$moments[1]
    ci_var[i] <- CI$epmoms$moments[2]^2 # notice that a variance is computed
    ci_skw[i] <- CI$epmoms$ratios[3]
    if(! quiet) cat(c(num.Fs-i,"-"),sep="")
  }
  if(! quiet) cat("\n")

  cis <- data.frame(nonexceed_prob=F,
                    lower=ci_low,
                    true=ci_tru,
                    upper=ci_hi,
                    lscale=ci_l2,
                    lcv=ci_l2/ci_tru,
                    mu=ci_mu,
                    var=ci_var)

  lmr <- data.frame(lambda1=ci_l1,
                    lambda2=ci_l2,
                    tau3=ci_t3,
                    tau4=ci_t4,
                    tau5=ci_t5)
  pmr <- data.frame(mu=ci_mu, var=ci_var, skw=ci_skw)
  if(expand == TRUE) {
    return(list(limits=cis, parent=para,
                edist=edist, elmoms=lmr, epmoms=pmr, epara=CI$epara,
                ifail=CI$ifail, ifailtext=CI$ifailtext))
  }
  else {
    return(cis)
  }
}
