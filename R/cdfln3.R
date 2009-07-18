"cdfln3" <-
function(x,para) {
    
    if(! are.parln3.valid(para)) return()

    XI <- para$para[1]
    U  <- para$para[2]
    A  <- para$para[3]

    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
      if(x <= 0) {
         f[i] <- NA
         next
      }
      Y <- (log(x[i]-XI) - U)/A
      f[i] <- pnorm(Y)
    }
    return(f)
}

