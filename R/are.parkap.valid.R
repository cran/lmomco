"are.parkap.valid" <-
function(para) {
    if(! is.kap(para)) return(FALSE)
    # The ifail==2 case is above the Generalized Logistic Line
    # if the length is one, then the ifail is present and we
    # should test it.
    if(length(para$ifail) == 1 && para$ifail == 2) return(FALSE)
    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]
    if(A <= 0) {
      warning("Parameters are invalid")
      return(FALSE)
    }
    if(G <= -1) {
      warning("Parameters are invalid")
      return(FALSE)
    }
    if(H < 0 && G*H <= -1) {
      warning("Parameters are invalid")
      return(FALSE)
    }
    return(TRUE)
}

