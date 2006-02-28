"quagld" <-
function(f,gldpara,paracheck=TRUE) {

    if(paracheck == TRUE) {
      if(! are.pargld.valid(gldpara)) return()
    }

    La1 <- gldpara$para[1]
    La2 <- gldpara$para[2]
    La3 <- gldpara$para[3]
    La4 <- gldpara$para[4]

    if(f <= 0 || f >= 1) {
      if(f == 0) return(La1-La2)
      if(f == 1) return(La1+La2)
    }
    return(La1 + La2*(f**La3 - (1-f)**La4))
}
