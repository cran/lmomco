"expect.max.ostat" <-
function(n, para=NULL, cdf=NULL, pdf=NULL,
         j=NULL, lower=-Inf, upper=Inf, ...) {

   if(is.null(j)) j <- n
   if(j > n)         stop("j can not be greater than n")
   if(is.null(para)) stop("parameter list of lmomco not specified")
   if(is.null(cdf))  stop("cdf function of lmomco not specified")
   if(is.null(pdf))  stop("pdf function of lmomco not specified")

   "fn" <- function(x) {
      F <- cdf(x, para=para, ...)
      a <- F^(j-1); b <- (1-F)^(n-j)
      return(x*a*b*pdf(x, para=para))
   }
   tmp <- NULL
   try(tmp <- integrate(fn, lower, upper))
   if(is.null(tmp)) return(NA)
   B <- beta(j,n-j+1)
   return(tmp$value/B)
}

"expect.min.ostat" <-
function(n, ...) {
  return(expect.max.ostat(n, j=1, ...))
}

