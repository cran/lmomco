"lmom2pwm" <-
function(lmom) {
   mylmom <- lmom
   if(is.list(lmom)) {
     if(length(lmom$lambdas) & length(lmom$ratios)) {
       if(length(lmom$lambdas) >= 5 & length(lmom$ratios) >= 5) {
         mylmom$L1 <- lmom$lambdas[1]
         mylmom$L2 <- lmom$lambdas[2]
         mylmom$TAU3 <- lmom$ratios[3]
         mylmom$TAU4 <- lmom$ratios[4]
         mylmom$TAU5 <- lmom$ratios[5]
       }
       else {
         warning("Not enough Lamdas or Ratios")
         return(NULL)
       }
     }
     else {
       if(! is.null(lmom$L1)   |  ! is.null(lmom$L2)   |
          ! is.null(lmom$TAU3) |  ! is.null(lmom$TAU4) |
          ! is.null(lmom$TAU5)) {
         warning("Incomplete list of L-moments")
         return(NULL)
        }
     }
   }
   p0 = mylmom$L1
   p1 = 0.5*(mylmom$L2+p0)
   p2 = (1/6)*(mylmom$L2*mylmom$TAU3+6*p1-p0)
   p3 = (1/20)*(mylmom$L2*mylmom$TAU4+30*p2-12*p1+p0)
   p4 = (1/70)*(mylmom$L2*mylmom$TAU5+140*p3-90*p2+20*p1-p0)
   z <- list(betas = c(p0,p1,p2,p3,p4), source='lmom2pwm')
   return(z)
}

