"lmom.test.exp" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parexp(lmom)
   cat("EXPONENTIAL DISTRIBUTION PARAMETERS\n")
   str(para)
   lmompara <- lmomexp(para)
   Q50 <- quaexp(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfexp(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}

