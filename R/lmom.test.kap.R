"lmom.test.kap" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parkap(lmom)
   cat("GENERALIZED KAPPA DISTRIBUTION PARAMETERS\n")
   str(para)
   lmompara <- lmomkap(para)
   Q50 <- quakap(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfkap(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}

