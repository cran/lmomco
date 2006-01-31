"quaexp" <-
function(f,para) {
    if(! are.parexp.valid(para)) return()
    U <- para$para[1]
    A <- para$para[2]
    if(f <= 0 || f >= 1) {
      warning("Nonexceedance probability is invalid")
      return()
    }
    return(U-A*log(1-f))
}

