"rrmvarlmomco"  <- function(f,para) {
    if(! are.par.valid(para)) return()
    Du <- vector(mode="numeric", length=length(f))
    for(i in 1:length(f)) {
       if(f[i] == 0) { Du[i] <- 0; next }
       tmp <- NULL
       "afunc" <- function(p) {
          return(rrmlmomco(p,para)^2)
       }
       try(tmp <- integrate(afunc, 0, f[i]))
       Du[i] <- ifelse(is.null(tmp), NA, tmp$value/f[i])
    }
    return(Du)
}

