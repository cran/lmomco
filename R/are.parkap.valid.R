"are.parkap.valid" <-
function(para) {
    if(! is.kap(para)) return(FALSE)
    if(para$ifail == 2) return(FALSE)
    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]
    if(A <= 0) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    if(G <= -1) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    if(H < 0 & G*H <= -1) {
      warning("Parameters are invalid.")
      return(FALSE)
    }
    return(TRUE)
}

