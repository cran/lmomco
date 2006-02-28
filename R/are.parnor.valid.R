"are.parnor.valid" <-
function(para) {
    if(! is.nor(para)) return(FALSE)
    sd <- para$para[2]
    if(sd <= 0) {
      warning("Parameters are invalid")
      return(FALSE)
    }
    return(TRUE)
}

