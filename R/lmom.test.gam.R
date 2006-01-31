"lmom.test.gam" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargam(lmom)
   print("GAMMA DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgam(para)
   Q50 <- quagam(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgam(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

