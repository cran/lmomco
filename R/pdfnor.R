"pdfnor" <-
function(x,para) {
    if(! are.parnor.valid(para)) return()
    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
      f[i] <- dnorm(x[i],mean = para$para[1], sd = para$para[2])
    }
    return(f)
}

