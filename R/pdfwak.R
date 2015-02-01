"pdfwak" <-
function(x,para) {

    if(! are.parwak.valid(para)) return()

    XI <- para$para[1]
    A  <- para$para[2]
    B  <- para$para[3]
    C  <- para$para[4]
    D  <- para$para[5]

    sup <- supdist(para, trapNaN=TRUE)
      low <- sup$support[1];   hi <- sup$support[2]
    low.is.finite <- sup $finite[1]; hi.is.finite <- sup$finite[2]

    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      Fc   <- 1 - cdfwak(x[i],para)
      if(low.is.finite & x[i] < low) {
         f[i] <- 0
      } else if(hi.is.finite & x[i] > hi){
         f[i] <- 0
      } else {
         tmp  <- A*Fc^(B - 1) + C*Fc^(-D - 1)
         f[i] <- 1/tmp
      }
    }
    return(f)
}

