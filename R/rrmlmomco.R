"rrmlmomco" <- function(f,para) {
    if(! are.par.valid(para)) return()
    Qu <- par2qua(f,para,paracheck=FALSE)
    Ru <- vector(mode="numeric", length=length(f))
    for(i in 1:length(f)) {
       if(f[i] == 0) { Ru[i] <- 0; next }
       tmp <- NULL
       "afunc" <- function(p) {
          return(par2qua(p,para,paracheck=FALSE))
       }
       try(tmp <- integrate(afunc, 0, f[i]))
       Ru[i] <- ifelse(is.null(tmp), NA, Qu[i] - tmp$value/f[i])
    }
    return(Ru)
}

