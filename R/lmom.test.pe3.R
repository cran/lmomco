"lmom.test.pe3" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parpe3(lmom)
   print("PEARSON TYPE III DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmompe3(para)
   Q50 <- quape3(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfpe3(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

