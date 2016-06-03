"mps2par" <-
function(x, type, para.int=NULL, silent=TRUE, ...) {
  "afunc" <- function(para, ...) {
     lmomco.para <- vec2par(para, type=type)
     if(is.null(lmomco.para)) return(Inf) # trap if bad parameters
     uu <- sort(c(0,plmomco(x, lmomco.para),1)) # padding the edges
     dd <- diff(uu) # the deltas, length shrinks by one
     #print(-sum(log(dd)))
     -sum(log(dd))
  }
  if(is.null(para.int)) {
     lmr <- lmoms(x)
     para.int <- lmom2par(lmr, type=type)
  }
  if(is.null(para.int)) {
     warning("could not estimate initial parameters via L-moments")
     return(NULL)
  }
  rt <- NULL
  try(rt <- optim(para.int$para, afunc, ...), silent=silent)
  if(is.null(rt)) {
     warning("optim() attempt is NULL")
     return(NULL)
  } else {
     lmomco.para <- vec2par(rt$par, type=type)
     lmomco.para$MoranStatistic <- rt$value
     lmomco.para$optim <- rt
     return(lmomco.para)
  }
}
