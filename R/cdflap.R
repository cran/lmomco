"cdflap" <-
function(x,para) {
    if(! are.parlap.valid(para)) return()

    XI <- para$para[1]
    A  <- para$para[2]

    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
      my.x  <- x[i]
      Y <- (my.x - XI)/A
      if(my.x <= XI) {
         f[i] <- 0.5*exp(Y)
      } else {
         f[i] <- 1 - 0.5*exp(-Y)
      }
    }
    return(f)
}

