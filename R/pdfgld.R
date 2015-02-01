"pdfgld" <-
function(x,gldpara,paracheck=TRUE) {

    # Check that the parameters are valid one time
    # then use the paracheck switch on quagld for
    # an extreme speed up on this algorithm.
    if(paracheck == TRUE) {
      if(! are.pargld.valid(gldpara)) return()
    }

    L2 <- gldpara$para[2]
    L3 <- gldpara$para[3]
    L4 <- gldpara$para[4]

    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      F <- cdfgld(x[i],gldpara,paracheck=FALSE)
      f[i] <- 1/((L3*F^(L3-1) + L4*(1-F)^(L4-1))*L2)
    }
    return(f)
}
