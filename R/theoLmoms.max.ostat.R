"theoLmoms.max.ostat" <-
function(para=NULL, cdf=NULL, pdf=NULL, nmom=5, ...) {

   if(is.null(para)) stop("parameter list of lmomco not specified")
   if(is.null(cdf))  stop("cdf function of lmomco not specified")
   if(is.null(pdf))  stop("pdf function of lmomco not specified")

   enn <- vector(mode="numeric", length=nmom)
   lms <- lmr <- enn
   for(r in 1:nmom) {
     enn[r] <- expect.max.ostat(r, para=para, cdf=cdf, pdf=pdf, ...)
     series <- 0
     for(k in r:1) {
       term <- (-1)^(r-k)*k^(-1)*choose(r-1,k-1)*choose(r+k-2,k-1)
       series <- series + term*enn[k]
     }
     lms[r] <- sum(series)
   }
   if(nmom >= 1) lmr[1] <- NA
   if(nmom >= 2) lmr[2] <- lms[2]/lms[1]
   if(nmom >= 3) {
      lmr[3:nmom] <- lms[3:nmom]/lms[2]
   }
   z <- list(lambdas=lms,
             ratios=lmr,
             trim=NULL,
             lefttrim=NULL,
             righttrim=NULL,
             source="theoLmoms.max.ostat")
   return(z)
}

