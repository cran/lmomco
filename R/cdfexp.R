"cdfexp" <-
function(x,para) {
    if(! are.parexp.valid(para)) return()
    U <- para$para[1]
    A <- para$para[2]
    Y <- (x-U)/A
    if(Y <= 0) return(0)
    return(1-exp(-Y))
}

