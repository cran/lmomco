"lmom.test.revgum" <-
function(data,zeta=1) {
   lmom <- lmom.ub(data)
   para <- parrevgum(lmom,zeta=zeta)
   cat("REVERSE GUMBEL DISTRIBUTION PARAMETERS\n")
   lmompara <- lmomrevgum(para)
   Q50 <- quarevgum(0.5,para)
   cat(c("Computed median=",Q50,"\n"),sep="")
   P50 <- cdfrevgum(Q50,para)
   cat(c("Nonexceedance of computed median=",P50,"\n"),sep="")
   lmom.diff(lmompara,lmom)
}
