"qualn3" <-
function(f,para) {
    
    if(! are.parln3.valid(para)) return()

    ZETA <- para$para[1]
    U  <- para$para[2]
    A  <- para$para[3]

    x <- vector(mode="numeric")
    for(i in seq(1,length(f))) {
      x[i] <- exp(qnorm(f[i])*A + U) + ZETA
    }
    return(x)
}

