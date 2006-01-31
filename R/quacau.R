"quacau" <-
function(f,para) {

    if(! are.parcau.valid(para)) return()

    U <- para$para[1]
    A <- para$para[2]

    if(f == 1) return(Inf)
    if(f == 0) return(-Inf)

    if(f == 0.5) return(U)
    return(U + A*tan(pi*(f-0.5)))
}
