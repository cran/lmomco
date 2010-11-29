"parrice" <-
function(lmom, checklmom=TRUE) {
   para <- vector(mode="numeric", length=2)
   names(para) <- c("nu","alpha")
   if(length(lmom$L1) == 0) { # convert to named L-moments
     lmom <- lmorph(lmom)     # nondestructive conversion!
   }
   if(checklmom & ! are.lmom.valid(lmom)) {
     warning("L-moments are invalid")
     return()
   }

   L1  <- lmom$L1
   LCV <- lmom$LCV
   if(is.null(L1) | is.null(lmom$LCV)) {
      stop("NULL L-moments")
   }
   RiceTable <- .lmomcohash$RiceTable
   if(LCV >  max(RiceTable$LCV)) {
      warning("LCV too big for Rice distribution (greater than Rayleigh)")
      return()
   }
   if(LCV <  min(RiceTable$LCV)) {
      warning("LCV too small for Rice distribution as implemented by lmomco")
      return()
   }
   SNR  <- approx(RiceTable$LCV, RiceTable$SNR, xout=LCV)$y
   G    <- approx(RiceTable$LCV, RiceTable$G,   xout=LCV, rule=1:2)$y
   A    <- L1/G
   V    <- A*SNR
   para[1] <- V
   para[2] <- A
   return(list(type='rice', para=para, source="parrice"))
}
