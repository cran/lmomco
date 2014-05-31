"cdflmrq" <-
function(x,para, paracheck=FALSE) {
    if(! are.parlmrq.valid(para)) return()
    SMALL  <- 1E-12
    SMALLo <- 1 - SMALL
    f <- vector(mode="numeric", length=length(x))
    afunc <- function(f, ax=NULL) {
        err <- qualmrq(f, para, paracheck=FALSE) - ax
        return(err)
    }
    for(i in seq(1,length(x))) {
      ax <- x[i]
      if(! is.finite(ax)) {
         f[i] <- 1
      } else if(ax == 0) {
         f[i] <- 0
      }
      else {
         rt <- NULL
         try(rt <- uniroot(afunc, lower=SMALL, upper=SMALLo, ax=ax), silent=FALSE)
         f[i] <- ifelse(is.null(rt), NA, rt$root)
      }
    }
    return(f)
}



