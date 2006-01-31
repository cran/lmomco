"lmom.test.gev" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargev(lmom)
   print("GENERALIZED EXTREME VALUE DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgev(para)
   Q50 <- quagev(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgev(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

