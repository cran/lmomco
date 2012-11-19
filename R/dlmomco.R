"dlmomco" <-
function(x,para) {
    if(! are.par.valid(para)) return()
    f <- par2pdf(x,para,paracheck=FALSE)
    return(f)
}
