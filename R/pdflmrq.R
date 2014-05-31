"pdflmrq" <-
function(x,para) {

    if(! are.parlmrq.valid(para)) return()

    U <- para$para[1]
    A <- para$para[2]
    UpA <- U - A

    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
       F <- cdflmrq(x[i], para, paracheck=FALSE)
       f[i] <- (1 - F)/(2*A*F + UpA)
    }
    return(f)
}

