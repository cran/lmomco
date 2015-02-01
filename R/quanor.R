"quanor" <-
function(f,para,paracheck=TRUE) {
    if(! check.fs(f)) return()
    if(paracheck == TRUE) {
      if(! are.parnor.valid(para)) return()
    }
    x <- vector(mode="numeric", length=length(f))
    for(i in seq(1,length(f))) {
      x[i] <- qnorm(f[i],mean = para$para[1], sd = para$para[2])
    }
    return(x)
}

