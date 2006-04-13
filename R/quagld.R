"quagld" <-
function(f,gldpara,paracheck=TRUE) {

    if(paracheck == TRUE) {
      if(! are.pargld.valid(gldpara)) return()
    }

    La1 <- gldpara$para[1]
    La2 <- gldpara$para[2]
    La3 <- gldpara$para[3]
    La4 <- gldpara$para[4]

    x <- vector(mode="numeric")
    for(i in seq(1,length(f))) {
      if(f[i] <= 0 || f[i] >= 1) {
        if(f[i] == 0) { x[i] <- La1-La2; next }
        if(f[i] == 1) { x[i] <- La1+La2; next }
      }
      x[i] <- La1 + La2*(f[i]**La3 - (1-f[i])**La4)
    }
    return(x)
}
