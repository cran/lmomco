"lmom.test.nor" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parnor(lmom)
   print("NORMAL DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomnor(para)
   Q50 <- quanor(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfnor(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

