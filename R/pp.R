"pp" <-
function(x, a=0, sort=TRUE) {
    if(a < 0 | a > 0.50) {
      warning("Plotting position parameter is invalid, not in [0,0.5]")
      return()
    }
    
    denom <- length(x) + 1 - 2*a
    ranks <- rank(x, ties.method="first")
    
    if(sort) {
      return( (sort(ranks) - a) / denom)
    } else {
      return(      (ranks  - a) / denom)
    }
}
