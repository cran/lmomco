"cdfgam" <-
function(x,para) {
    if(! are.pargam.valid(para)) return()
    if(x <= 0) return(0)
    ALPHA <- para$para[1] 
    BETA  <- para$para[2] 
    return(pgamma(x,ALPHA,scale=BETA))
}

