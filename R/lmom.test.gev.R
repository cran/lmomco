"lmom.test.gev" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargev(lmom)
   cat("GENERALIZED EXTREME VALUE DISTRIBUTION PARAMETERS\n")
   str(para)
   lmompara <- lmomgev(para)
   Q50 <- quagev(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfgev(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}

