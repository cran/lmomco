"lmom.test.kap" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parkap(lmom)
   print("GENERALIZED KAPPA DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomkap(para)
   Q50 <- quakap(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfkap(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

