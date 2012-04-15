"is.aep" <-
function(para) {
    if(para$type != "aep") {
      warning("Parameters are not Asymmetric Exponential parameters.")
      return(FALSE)
    }
    return(TRUE)
}
