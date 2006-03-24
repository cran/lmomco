"are.parcau.valid" <-
function(para,nowarn=FALSE,...) {
    if(! is.cau(para)) return(FALSE)
    U <- para$para[1] 
    A <- para$para[2] 
    #if() {
    #  warning("Parameters are invalid")
    #  return(FALSE)
    #}
    return(TRUE)
}

