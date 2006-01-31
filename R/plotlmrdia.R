"plotlmrdia" <-
function(lmr, 
         nopoints=FALSE,
	 nolines=FALSE,
	 nolimits=FALSE,
	 nogev=FALSE,
	 noglo=FALSE,
	 nogpa=FALSE,
	 nope3=FALSE,
	 nogno=FALSE,
	 noexp=FALSE,
	 nonor=FALSE,
	 nogum=FALSE,
	 nouni=FALSE, ...) {
   plot(lmr$limits, xlab = "L-SKEW", ylab = "L-KURTOSIS", type = "n",
        ...)
   if(nolimits == FALSE) {
     lines(lmr$limits,lwd=2,col=8)
   }
   if(nolines == FALSE) {
     if(nogev == FALSE) lines(lmr$gev, col=2,lty=2)
     if(noglo == FALSE) lines(lmr$glo, col=3)
     if(nogno == FALSE) lines(lmr$gno, col=4, lty=2)
     if(nogpa == FALSE) lines(lmr$gpa, col=4)
     if(nope3 == FALSE) lines(lmr$pe3, col=6)
   }
   if(nopoints == FALSE) {
     if(noexp == FALSE) points(lmr$exp,pch=16,col=2)
     if(nonor == FALSE) points(lmr$nor,pch=15,col=2)
     if(nogum == FALSE) points(lmr$gum,pch=17,col=2)
     if(nouni == FALSE) points(lmr$uniform,pch=18,cex=1.5,col=2)
   }
}
