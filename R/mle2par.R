"mle2par" <-
function(x, type, para.int=NULL, silent=TRUE, ...) {
  afunc <- function(para, x=NULL, ...) {
       lmomco.para <- vec2par(para, type=type)
       if(is.null(lmomco.para)) return(Inf)
       pdf <- par2pdf(x,lmomco.para) # pull into local scope, in case of later interception of problems
       sum(-log(pdf), na.rm=TRUE)
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
  try(rt <- optim(para.int$para, afunc, x=x, ...), silent=silent)
  if(is.null(rt)) {
     warning("optim() attempt is NULL")
     return(NULL)
  } else {
     lmomco.para <- vec2par(rt$par, type=type)
     lmomco.para$optim <- rt
     return(lmomco.para)
  }
}
