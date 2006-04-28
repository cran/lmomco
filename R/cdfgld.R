"cdfgld" <-
function(x,gldpara,paracheck=TRUE) {

    # Check that the parameters are valid one time
    # then use the paracheck switch on quagld for
    # an extreme speed up on this algorithm.
    if(paracheck == TRUE) {
      if(! are.pargld.valid(gldpara)) return()
    }


    # Use the machine's smallest and and 1 - smallest values
    # instead of 0 and 1 as certain parameters yield unstable
    # or strange infinite results at the 0,1 end points.
    # An example parameter vector is
    # x=-7.956441e+05 a=-7.901925e+05  k=6.871662e+01 h=-3.749302e-01
    # These parameters on one of my linux boxes produces
    # quagld(0,PAR) equal to -5451.6
    # quagld(1,PAR) equal to -1585837, yet at same time 0.99999999999 is ok.
    end.min <-   .Machine$double.eps
    end.max <- 1-.Machine$double.eps
    qua.min <- quagld(end.min,gldpara,paracheck=FALSE)
    qua.max <- quagld(end.max,gldpara,paracheck=FALSE)

    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
      #cat(c("Working on i=",i," and x[i]=",x[i],"\n"),sep="")
      QUAx <- x[i]
      if(QUAx <= qua.min) { f[i] <- end.min; next }
      if(QUAx >= qua.max) { f[i] <- end.max; next }
      
      fn <- function(F) {
        qua <- quagld(F,gldpara,paracheck=FALSE)
        val <- QUAx - qua
        #cat(c("Status: i=",i,"; x[i]=",QUAx,"; F=",F,"; and quagld=",qua,"\n"),sep="")
        return(val)
      }

      root <- uniroot(fn,c(end.min,end.max))
      f[i] <- root$root
    }
    return(f)
}

