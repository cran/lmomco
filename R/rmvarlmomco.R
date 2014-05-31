"rmvarlmomco"  <- function(f,para) {
    if(! are.par.valid(para)) return()
    Vu <- vector(mode="numeric", length=length(f))
    for(i in 1:length(f)) {
       if(f[i] == 1) { Vu[i] <- 0; next }
       tmp <- NULL
       "afunc" <- function(p) {
          return(rmlmomco(p,para)^2)
       }
       try(tmp <- integrate(afunc, f[i], 1))
       Vu[i] <- ifelse(is.null(tmp), NA, tmp$value/(1-f[i]))
    }
    return(Vu)
}

