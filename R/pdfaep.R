"pdfaep" <-
function(x, para, paracheck=TRUE) {

   if(paracheck == TRUE) {
     if(! are.paraep.valid(para)) return()
   }
    U <- para$para[1]
    A <- para$para[2]
    K <- para$para[3]
    H <- para$para[4]

    Z <- H*K / ( A * (1 + K*K) * gamma(1/H) )

    f <- vector(mode = "numeric")
    for(i in seq(1,length(x))) {
       Y   <-  abs(x[i] - U) / A
       f[i] <- Z * exp(-1 * (  K^sign(x[i] - U) * Y )^H )
    }
    return(f)
}
