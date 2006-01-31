"lmom.test.exp" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parexp(lmom)
   print("EXPONENTIAL DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomexp(para)
   Q50 <- quaexp(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfexp(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

