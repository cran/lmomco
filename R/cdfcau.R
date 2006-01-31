"cdfcau" <-
function(x,para) {
    if(! are.parcau.valid(para)) return()
    U <- para$para[1] 
    A <- para$para[2] 
    tmp <- (x - U)/A
    tmp <- (atan(tmp)/pi)+0.5
    return(tmp)
}

