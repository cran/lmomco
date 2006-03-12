"lmom.test.gum" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargum(lmom)
   cat("GUMBEL DISTRIBUTION PARAMETERS\n")
   str(para)
   lmompara <- lmomgum(para)
   Q50 <- quagum(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfgum(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}

