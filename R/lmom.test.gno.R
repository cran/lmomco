"lmom.test.gno" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargno(lmom)
   cat("GENERALIZED NORMAL DISTRIBUTION PARAMETERS\n")
   str(para)
   lmompara <- lmomgno(para)
   Q50 <- quagno(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfgno(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}

