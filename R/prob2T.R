"prob2T" <-
function(F) {
    if(F < 0 || F > 1) {
      warning("Invalid nonexceedance probability")
      return(NULL)
    }
    return(1/(1-F))
}
