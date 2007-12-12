"prob2T" <-
function(F) {
    if(any(F < 0) || any(F > 1)) {
      warning("Invalid nonexceedance probability")
      return(NULL)
    }
    return(1/(1-F))
}
