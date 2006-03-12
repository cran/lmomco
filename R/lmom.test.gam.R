"lmom.test.gam" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargam(lmom)
   cat("GAMMA DISTRIBUTION PARAMETERS\n")
   str(para)
   lmompara <- lmomgam(para)
   Q50 <- quagam(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfgam(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}

