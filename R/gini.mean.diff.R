"gini.mean.diff" <-
function(x) {
  x <- sort(x[! is.na(x)])
  n <- length(x)
  na <- seq((1-n),(n-1), by=2)
  gini <- (2/(n*(n-1)))*sum(na*x)
  z <- list(gini=gini,
            L2=gini/2,
            source="gini.mean.diff")
  return(z)
}
