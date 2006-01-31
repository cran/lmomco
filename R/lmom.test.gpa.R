"lmom.test.gpa" <-
function(data) {
   lmom <- lmom.ub(data)
   para <- pargpa(lmom)
   print("GENERALIZED PARETO DISTRIBUTION PARAMETERS")
   print(para)
   lmompara <- lmomgpa(para)
   Q50 <- quagpa(0.5,para)
   print(c('MEDIAN ',Q50))
   P50 <- cdfgpa(Q50,para)
   print(c('NONEXCEEDANCE OF COMPUTED MEDIAN ',P50))
   lmom.diff(lmompara,lmom)
}

