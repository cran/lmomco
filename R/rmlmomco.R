"rmlmomco" <- function(f,para) {
    if(! are.par.valid(para)) return()
    Qu <- par2qua(f,para,paracheck=FALSE)
    Mu <- vector(mode="numeric", length=length(f))
    for(i in 1:length(f)) {
       if(f[i] == 1) { Mu[i] <- 0; next }
       tmp <- NULL
       "afunc" <- function(p) {
          return(par2qua(p,para,paracheck=FALSE))
       }
       try(tmp <- integrate(afunc, f[i], 1))
       Mu[i] <- ifelse(is.null(tmp), NA, tmp$value/(1-f[i]) - Qu[i])
    }
    return(Mu)
}
