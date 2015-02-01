"pdflap" <-
function(x,para) {
    if(! are.parlap.valid(para)) return()

    XI <- para$para[1]
    A  <- para$para[2]

    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      Y  <- abs(x[i]-XI)/A
      f[i] <- 0.5/A * exp(-Y)
    }
    return(f)
}

