"lmom.test.wak" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- parwak(lmom)
   print("WAKEBY DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomwak(para)
   Q50 <- quawak(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfwak(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

