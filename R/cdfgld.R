"cdfgld" <-
function(x,gldpara) {

    # Check that the parameters are valid one time
    # then use the paracheck switch on quagld for
    # an extreme speed up on this algorithm.
    if(! are.pargld.valid(gldpara)) return()

    fn <- function(F) {
      return(x - quagld(F,gldpara,paracheck=FALSE))
    }
    root <- uniroot(fn,c(0,1))
    return(root$root)
}

