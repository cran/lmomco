"pdfwak" <-
function(x,para) {

    if(! are.parwak.valid(para)) return()

    XI <- para$para[1]
    A  <- para$para[2]
    B  <- para$para[3]
    C  <- para$para[4]
    D  <- para$para[5]

    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
      Fc   <- 1 - cdfwak(x[i],para)
      tmp  <- A*Fc^(B - 1) + C*Fc^(-D - 1)
      f[i] <- 1/tmp
    }
    return(f)
}

