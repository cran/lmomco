"quanor" <-
function(f,para) {
    if(! are.parnor.valid(para)) return()
    return(qnorm(f,mean = para$para[1], sd = para$para[2]))
}

