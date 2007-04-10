"lmom.test.wei" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parwei(lmom)
   cat("WEIBULL DISTRIBUTION PARAMETERS\n")
   str(para)
   lmompara <- lmomwei(para)
   Q50 <- quawei(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfwei(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}

