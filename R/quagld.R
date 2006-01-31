"quagld" <-
function(f,gldpara) {

    if(! are.pargld.valid(gldpara)) return()

    La1 <- gldpara$para[1]
    La2 <- gldpara$para[2]
    La3 <- gldpara$para[3]
    La4 <- gldpara$para[4]
    tmp <- 1/La2

    if(f <= 0 || f >= 1) {
      if(f == 0) return(La1-tmp)
      if(f == 1) return(La1+tmp)
    }
    return(La1 + tmp*(f**La3 - (1-f)**La4))
}
