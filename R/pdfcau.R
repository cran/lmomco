"pdfcau" <-
function(x,para) {
    if(! are.parcau.valid(para)) return()
    U <- para$para[1] 
    A <- para$para[2] 
 
    f <- vector(mode="numeric")
    for(i in seq(1,length(x))) {
      tmp <- (x[i] - U)/A
      tmp <- (pi*A*(1 + tmp^2))^(-1)
      f[i] = tmp
    }
    return(f)
}
