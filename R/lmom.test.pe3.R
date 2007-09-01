"lmom.test.pe3" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parpe3(lmom)
   cat("PEARSON TYPE III DISTRIBUTION PARAMETERS\n")
   str(para)
   lmompara <- lmompe3(para)
   Q50 <- quape3(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfpe3(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}

