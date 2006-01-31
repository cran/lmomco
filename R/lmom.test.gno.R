"lmom.test.gno" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargno(lmom)
   print("GENERALIZED NORMAL DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgno(para)
   Q50 <- quagno(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgno(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

