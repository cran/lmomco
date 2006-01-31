"quakap" <-
function(f,para) {
    if(! are.parkap.valid(para)) return()

    U <- para$para[1]
    A <- para$para[2]
    G <- para$para[3]
    H <- para$para[4]   
    if(f <= 0 || f >= 1) {
      if(f == 0) {
        if(H <= 0 & G  < 0) return(U+A/G)
        if(H >  0 & G != 0) return(U+A/G*(1-H^-G))
        if(H >  0 & G == 0) return(U+A*log(H))
        if(H <= 0 & G >= 0) {
          warning("argument to function invalid.")
          return()
        }
        stop("f is fine: should not be here in code execution.")
      }
      if(f == 1) {
        if(G <= 0) {
          warning("argument of function is invalid")
          return()
        }
        else {
          return(U+A/G)
        }
        stop("f=1: should not be here in code execution.")
      }
    }
    else {
      Y <- -log(f)
      if(H != 0) Y <- (1-exp(-H*Y))/H
      Y <- -log(Y)
      if(G != 0) Y <- (1-exp(-G*Y))/G
      return(U+A*Y)
    }
}

