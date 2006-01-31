"cdfnor" <-
function(x,para) {
    if(! are.parnor.valid(para)) return()
    return(pnorm(x,mean = para$para[1], sd = para$para[2]))
}

