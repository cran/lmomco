"supdist" <- function(para, trapNaN=FALSE, ...) {
   if(! are.par.valid(para, ...)) {
      warning("parameter object seems to to be invalid, returning NULL")
      return()
   }
   lo <- par2qua(0, para, ...)
   lo.e <- .Machine$sizeof.longdouble
   if(is.na(lo)) {
      trapNaN <- TRUE
      lo <- NaN
   }
   if(trapNaN) {
      if(is.nan(lo)) {
         while(is.nan(lo)) {
            if(lo.e == 4) break
            lo <- par2qua(0 + 10^(-lo.e), para, ...)
            if(is.na(lo)) lo <- NaN
            if(! is.nan(lo)) break
            lo.e <- lo.e - 1
         }
      } else {
         lo.e <- NA
      }
   } else {
      lo.e <- NA
   }
   hi <- par2qua(1, para, ...)
   hi.e <- .Machine$sizeof.longdouble
   if(is.na(hi)) {
      trapNaN <- TRUE
      hi <- NaN
   }
   if(trapNaN) {
      if(is.nan(hi)) {
         while(is.nan(hi)) {
            if(hi.e == 3) break
            hi <- par2qua(1 - 10^(-hi.e), para, ...)
            if(is.na(hi)) hi <- NaN
            if(! is.nan(hi)) break
            hi.e <- hi.e - 1
         }
      } else {
         hi.e <- NA
      }
   } else {
      hi.e <- NA
   }
   zz <- list(type    = para$type,
              support = c(          lo,            hi),
              fexpons = c(         -lo.e,         -hi.e),
              finite  = c(is.finite(lo), is.finite(hi)),
              source  = "supdist")
   return(zz)
}
