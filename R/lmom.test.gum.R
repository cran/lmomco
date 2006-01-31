"lmom.test.gum" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargum(lmom)
   print("GUMBEL DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgum(para)
   Q50 <- quagum(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgum(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

