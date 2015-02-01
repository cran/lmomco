"cdfcau" <-
function(x,para) {
    if(! are.parcau.valid(para)) return()
    U <- para$para[1] 
    A <- para$para[2] 
 
    f <- vector(mode="numeric", length=length(x))
    for(i in seq(1,length(x))) {
      tmp <- (x[i] - U)/A
      tmp <- (atan(tmp)/pi)+0.5
      f[i] = tmp
    }
    return(f)
}

