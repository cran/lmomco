"Lcomoment.Wk" <-
function(k, r, n) {
  # Following notation of Serfling and Xiao (2006)
  #   compute the Wk weight factor for kth L-moment
  Wk <- 0
  jn <- min(c(r-1,k-1))  # find the minimum for the loop end
  for(j in seq(0,jn)) {
    t1 <- (-1)**(k-1-j)
    t2 <- choose(k-1,j)
    t3 <- choose(k-1+j,j)
    t4 <- choose(n-1,j)
    t5 <- choose(r-1,j)
    Wk <- Wk + t1*t2*t3*t5/t4
  }
  return(Wk)
}
