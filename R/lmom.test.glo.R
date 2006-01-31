"lmom.test.glo" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parglo(lmom)
   print("GENERALIZED LOGISTIC DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomglo(para)
   Q50 <- quaglo(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfglo(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

