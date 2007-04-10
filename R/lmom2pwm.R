"lmom2pwm" <-
function(lmom) {
   p0 = lmom$L1
   p1 = 0.5*(lmom$L2+p0)
   p2 = (1/6)*(lmom$L2*lmom$TAU3+6*p1-p0)
   p3 = (1/20)*(lmom$L2*lmom$TAU4+30*p2-12*p1+p0)
   p4 = (1/70)*(lmom$L2*lmom$TAU5+140*p3-90*p2+20*p1-p0)
   z <- list(BETA0 = p0, BETA1 = p1, BETA2 = p2, BETA3 = p3, BETA4 = p4)
   return(z)
}

